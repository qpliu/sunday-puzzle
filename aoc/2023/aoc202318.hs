import Data.Map(Map,alter,empty,keys,keysSet)
import Data.Set(Set,fromList,insert,member,size,union)

parse :: String -> [((Char,Int),String)]
parse = map (parseLine . words) . lines
  where parseLine [[ch],n,'(':'#':color] = ((ch,read n),init color)

-- The input data looks small enough to brute force rather then integrating
-- along the perimeter.
-- Fits in 569×316 = 179804 cubic meters

trench :: [((Char,Int),String)] -> Map (Int,Int) [(Char,String)]
trench = snd . foldl dig ((0,0),empty)
  where
    dig ((x,y),grid) ((dir,n),color) = ((x+dx*n,y+dy*n),foldl digCube grid [(x+dx*i,y+dy*i) | i <- [0..n]])
      where
        dx | dir == 'R' = 1 | dir == 'L' = -1 | otherwise = 0
        dy | dir == 'D' = 1 | dir == 'U' = -1 | otherwise = 0
        digCube grid2 xy = alter (Just . maybe [(dir,color)] ((dir,color):)) xy grid2

interior :: Map (Int,Int) [(Char,String)] -> Set (Int,Int)
interior trnch = snd $ foldl fill (initOutside,fromList []) $ keys trnch
  where
    xmin = minimum $ map fst $ keys trnch
    xmax = maximum $ map fst $ keys trnch
    ymin = minimum $ map snd $ keys trnch
    ymax = maximum $ map snd $ keys trnch
    initOutside = fromList $ [(xmin-1,y) | y <- [ymin-1..ymax+1]] ++ [(xmax+1,y) | y <- [ymin-1..ymax+1]] ++ [(x,ymin-1) | x <- [xmin..xmax]] ++ [(x,ymax+1) | x <- [xmin..xmax]]

    fill (outside,inside) (x,y) = foldl flood (outside,inside) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
    flood (outside,inside) xy
      | xy `member` outside || xy `member` inside || xy `member` keysSet trnch = (outside,inside)
      | otherwise = f (fromList []) [xy]
      where
        f current [] = (outside,union current inside)
        f current (cxy@(cx,cy):rest)
          | cxy `member` outside = (union outside current,inside)
          | cxy `member` keysSet trnch || cxy `member` current = f current rest
          | otherwise = f (insert cxy current) ((cx+1,cy):(cx-1,cy):(cx,cy+1):(cx,cy-1):rest)

-- Too slow
resultSlow :: String -> Int
resultSlow input = size (interior trnch) + size (keysSet trnch)
  where
    trnch = trench $ parse input

result :: String -> Int
result = area . trench2 . map fst . parse

testData :: String
testData = unlines [
    "R 6 (#70c710)",
    "D 5 (#0dc571)",
    "L 2 (#5713f0)",
    "D 2 (#d2c081)",
    "R 2 (#59c680)",
    "D 2 (#411b91)",
    "L 5 (#8ceee2)",
    "U 2 (#caa173)",
    "L 1 (#1b58a2)",
    "U 2 (#caa171)",
    "R 2 (#7807d2)",
    "U 3 (#a77fa3)",
    "L 2 (#015232)",
    "U 2 (#7a21e3)"
    ]

test :: ()
test
  | result testData /= 62 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/18.txt"

parse2 :: String -> [(Char,Int)]
parse2 = map (parseLine . words) . lines
  where
    parseLine [_,_,'(':'#':a:b:c:d:e:f:_] = (dir f,read ['0','x',a,b,c,d,e])
    dir '0' = 'R'
    dir '1' = 'D'
    dir '2' = 'L'
    dir '3' = 'U'

-- Looking at a picture of the trench in my input data, the trench does not
-- intersect itself, so nothing is enclosed multiple times.
-- There are no U turns.
-- My input data starts left, making a clockwise loop, finishing going up.

-- For clockwise, to include the trench
-- When moving right, the reference is the upper left corner
-- When moving down, the reference is the upper right corner
-- When moving left, the reference is the lower right corner
-- When moving up, the reference is the lower left corner
--   R---D
--   |   |
--   U---L

trench2 :: [(Char,Int)] -> [(Int,Int)]
trench2 = follow (0,0) 'U'
  where
    follow xy@(0,0) 'U' [] = [(0,0)]
    follow (x,y) 'R' (('U',n):rest) = (x,y) : follow (x,y-n+1) 'U' rest
    follow (x,y) 'R' (('D',n):rest) = (x+1,y) : follow (x+1,y+n) 'D' rest
    follow (x,y) 'L' (('U',n):rest) = (x-1,y) : follow (x-1,y-n) 'U' rest
    follow (x,y) 'L' (('D',n):rest) = (x,y) : follow (x,y+n-1) 'D' rest
    follow (x,y) 'U' (('L',n):rest) = (x,y) : follow (x-n+1,y) 'L' rest
    follow (x,y) 'U' (('R',n):rest) = (x,y-1) : follow (x+n,y-1) 'R' rest
    follow (x,y) 'D' (('L',n):rest) = (x,y+1) : follow (x-n,y+1) 'L' rest
    follow (x,y) 'D' (('R',n):rest) = (x,y) : follow (x+n-1,y) 'R' rest
    follow xy dir rest = error (show (xy,dir,rest))

area :: [(Int,Int)] -> Int
area pts = sum (zipWith3 f (drop 1 pts ++ take 1 pts) pts (last pts:init pts)) `div` 2
  where f (xm,_) (_,y) (xp,_) = y*(xp-xm)

result2 :: String -> Int
result2 = area . trench2 . parse2

test2 :: ()
test2
  | result2 testData /= 952408144115 = error "a"
  | otherwise = ()


part2 :: IO Int
part2 = fmap result2 $ readFile "input/18.txt"
