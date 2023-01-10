{-
--- Day 6: Chronal Coordinates ---

The device on your wrist beeps several times, and once again you feel like
you're falling.

"Situation critical," the device announces. "Destination indeterminate. Chronal
interference detected. Please specify new target coordinates."

The device then produces a list of coordinates (your puzzle input). Are they
places it thinks are safe or dangerous? It recommends you check manual page
729. The Elves did not give you a manual.

If they're dangerous, maybe you can minimize the danger by finding the
coordinate that gives the largest distance from the other points.

Using only the Manhattan distance, determine the area around each coordinate by
counting the number of integer X,Y locations that are closest to that
coordinate (and aren't tied in distance to any other coordinate).

Your goal is to find the size of the largest area that isn't infinite. For
example, consider the following list of coordinates:

| 1, 1
| 1, 6
| 8, 3
| 3, 4
| 5, 5
| 8, 9

If we name these coordinates A through F, we can draw them on a grid, putting
0,0 at the top left:

| ..........
| .A........
| ..........
| ........C.
| ...D......
| .....E....
| .B........
| ..........
| ..........
| ........F.

This view is partial - the actual grid extends infinitely in all directions.
Using the Manhattan distance, each location's closest coordinate can be
determined, shown here in lowercase:

| aaaaa.cccc
| aAaaa.cccc
| aaaddecccc
| aadddeccCc
| ..dDdeeccc
| bb.deEeecc
| bBb.eeee..
| bbb.eeefff
| bbb.eeffff
| bbb.ffffFf

Locations shown as . are equally far from two or more coordinates, and so they
don't count as being closest to any.

In this example, the areas of coordinates A, B, C, and F are infinite - while
not shown here, their areas extend forever outside the visible grid. However,
the areas of coordinates D and E are finite: D is closest to 9 locations, and E
is closest to 17 (both including the coordinate's location itself). Therefore,
in this example, the size of the largest area is 17.

What is the size of the largest area that isn't infinite?
-}

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
