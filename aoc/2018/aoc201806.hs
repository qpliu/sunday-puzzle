import Data.Char(isDigit)
import Data.List(delete)

parse :: String -> [(Int,Int)]
parse = p . words
  where
    p (x:y:rest) = (read (filter isDigit x), read (filter isDigit y)):p rest
    p _ = []

bounds :: [(Int,Int)] -> ((Int,Int),(Int,Int))
bounds coords = ((minimum xs,minimum ys),(maximum xs,maximum ys))
  where (xs,ys) = (map fst coords,map snd coords)

dist :: (Int,Int) -> (Int,Int) -> Int
dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

areaSize :: [(Int,Int)] -> (Int,Int) -> Maybe Int
areaSize allCoords xy@(x,y) = maybeSum 1 [county pred (y-1), county succ (y+1), countx y pred (x-1), countx y succ (x+1)]
  where
    ((xmin,ymin),(xmax,ymax)) = bounds allCoords
    coords = delete xy allCoords
    county next yy
      | yy < ymin || yy > ymax = Nothing
      | dist xy (x,yy) >= minimum (map (dist (x,yy)) coords) = Just 0
      | otherwise = maybeSum 1 [countx yy pred (x-1), countx yy succ (x+1), county next (next yy)]
    countx yy next xx
      | xx < xmin || xx > xmax = Nothing
      | dist xy (xx,yy) >= minimum (map (dist (xx,yy)) coords) = Just 0
      | otherwise = maybeSum 1 [countx yy next (next xx)]
    maybeSum total [] = Just total
    maybeSum total (Nothing:_) = Nothing
    maybeSum total (Just n:rest) = maybeSum (total+n) rest

testData :: String
testData = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"

test :: ()
test
  | map (areaSize coords) coords /= [Nothing,Nothing,Nothing,Just 9,Just 17,Nothing] = error "a"
  | otherwise = ()
  where coords = parse testData

largestArea :: [(Int,Int)] -> Maybe Int
largestArea coords = maximum $ map (areaSize coords) coords

part1 :: IO (Maybe Int)
part1 = fmap (largestArea . parse) $ readFile "input/06.txt"

totalDist :: [(Int,Int)] -> (Int,Int) -> Int
totalDist coords xy = sum $ map (dist xy) coords

-- In my input data, every point on the bounding rectangle has a total
-- distance of more than 10000, so I can ignore points outside the
-- bounding rectangle.  (If they weren't, each step away from the bounding
-- rectangle adds the number of coordinates to the total distance.)

safeArea :: Int -> [(Int,Int)] -> Int
safeArea tooFar coords = length [() | x <- [xmin..xmax], y <- [ymin..ymax], totalDist coords (x,y) < tooFar]
  where ((xmin,ymin),(xmax,ymax)) = bounds coords

test2 :: ()
test2
  | safeArea 32 (parse testData) /= 16 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (safeArea 10000 . parse) $ readFile "input/06.txt"
