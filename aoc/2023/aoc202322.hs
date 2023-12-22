import Data.Char(isDigit)
import Data.List(nub,sortBy)
import Data.Map(Map,adjust,fromList,insert,keys,member,size,toList,(!))

type XY = (Int,Int)
type XYZ = (Int,Int,Int)
type Brick = (XYZ,XYZ)

parse :: String -> [Brick]
parse = sortBy compareBricks . map (toCoords . map read . words . map keepDigits) . lines
  where
    keepDigits ch | isDigit ch = ch | otherwise = ' '
    toCoords [x1,y1,z1,x2,y2,z2] = ((x1,y1,z1),(x2,y2,z2))

-- In my input, x,y is in a 10×10 area

-- If a brick is supported multiple bricks, it's not possible for one of
-- the supporting bricks to (indirectly) support another supporting brick,
-- since all bricks are lines of cubes, aligned along one of the X, Y, or Z
-- axis.

-- Thus, if every brick supported by brick A is supported by multiple bricks,
-- the brick A can be safely disintegrated.

atXY :: XY -> Brick -> Bool
atXY (x,y) ((x1,y1,_),(x2,y2,_)) =
  ((x1 <= x && x <= x2) || (x1 >= x && x >= x2)) &&
  ((y1 <= y && y <= y2) || (y1 >= y && y >= y2))

compareBricks :: Brick -> Brick -> Ordering
compareBricks ((x1,y1,z1),(x2,y2,z2)) ((xx1,yy1,zz1),(xx2,yy2,zz2)) = compare (min z1 z2) (min zz1 zz2)

shadow :: Brick -> [XY]
shadow ((x1,y1,_),(x2,y2,_)) = [(x,y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

height :: Brick -> Int
height ((_,_,z1),(_,_,z2)) = 1 + abs (z1-z2)

stackBricks :: [Brick] -> (Map Brick [Brick],Map Brick [Brick])
stackBricks bricks =
    snd $ foldl stackBrick (initialStack,(initialSupporters,initialSupportees)) bricks
  where
    ground = ((0,0,0),(0,0,0))
    initialStack = fromList [((x,y),(0,ground)) | x <- [0..9], y <- [0..9]]
    initialSupporters = fromList []
    initialSupportees = fromList ((ground,[]) : (zip bricks (repeat [])))
    
    stackBrick :: (Map XY (Int,Brick),(Map Brick [Brick],Map Brick [Brick])) -> Brick ->  (Map XY (Int,Brick),(Map Brick [Brick],Map Brick [Brick]))
    stackBrick (stack,(supporters,supportees)) brick =
        (newStack,(newSupporters,newSupportees))
      where
        z = maximum [fst (stack!xy) | xy <- shadow brick]
        newStack = foldl (\ tmpStack xy -> insert xy (z+height brick,brick) tmpStack) stack $ shadow brick
        support = nub [brk | xy <- shadow brick, (top,brk) <- [stack!xy], top == z]
        newSupporters = insert brick support supporters
        newSupportees = foldr (adjust (brick:)) supportees support

result :: String -> Int
result input = length $ filter safeDisint bricks
  where
    bricks = parse input
    (supporters,supportees) = stackBricks bricks
    safeDisint brick = and [length (supporters!brk) > 1 | brk <- supportees!brick]

testData :: String
testData = unlines [
    "1,0,1~1,2,1",
    "0,0,2~2,0,2",
    "0,2,3~2,2,3",
    "0,0,4~0,2,4",
    "2,0,5~2,2,5",
    "0,1,6~2,1,6",
    "1,1,8~1,1,9"
    ]

test :: ()
test
  | result testData /= 5 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/22.txt"

-- brute-force n² should be doable, n = 1479, n² = 2187441
fallCount :: (Map Brick [Brick],Map Brick [Brick]) -> Brick -> Int
fallCount (supporters,supportees) brick =
    size (fall (fromList [(brick,())]) (fromList []) [brick]) - 1
  where
    fall fallen processed [] = fallen
    fall fallen processed (brick:bricks)
      | brick `member` processed = fall fallen processed bricks
      | otherwise = fall (foldr (flip insert ()) fallen newFallen) (insert brick () processed) (bricks ++ filter (not . (`member` processed)) newFallen)
      where
        newFallen = [brk | brk <- supportees!brick, all (`member` fallen) (supporters!brk)]

result2 :: String -> Int
result2 input = sum $ map (fallCount (supporters,supportees)) bricks
  where
    bricks = parse input
    (supporters,supportees) = stackBricks bricks

test2 :: ()
test2
  | result2 testData /= 7 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/22.txt"
