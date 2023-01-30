{-
--- Day 5: Hydrothermal Venture ---

You come across a field of hydrothermal vents on the ocean floor! These vents
constantly produce large, opaque clouds, so it would be best to avoid them if
possible.

They tend to form in lines; the submarine helpfully produces a list of nearby
lines of vents (your puzzle input) for you to review. For example:

| 0,9 -> 5,9
| 8,0 -> 0,8
| 9,4 -> 3,4
| 2,2 -> 2,1
| 7,0 -> 7,4
| 6,4 -> 2,0
| 0,9 -> 2,9
| 3,4 -> 1,4
| 0,0 -> 8,8
| 5,5 -> 8,2

Each line of vents is given as a line segment in the format x1,y1 -> x2,y2
where x1,y1 are the coordinates of one end the line segment and x2,y2 are the
coordinates of the other end. These line segments include the points at both
ends. In other words:

 - An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
 - An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
 - For now, only consider horizontal and vertical lines: lines where either x1
   = x2 or y1 = y2.

So, the horizontal and vertical lines from the above list would produce the
following diagram:

| .......1..
| ..1....1..
| ..1....1..
| .......1..
| .112111211
| ..........
| ..........
| ..........
| ..........
| 222111....

In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9.
Each position is shown as the number of lines which cover that point or . if no
line covers that point. The top-left pair of 1s, for example, comes from 2,2 ->
2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9
-> 2,9.

To avoid the most dangerous areas, you need to determine the number of points
where at least two lines overlap. In the above example, this is anywhere in the
diagram with a 2 or larger - a total of 5 points.

Consider only horizontal and vertical lines. At how many points do at least two
lines overlap?
-}

import Data.Array(range)
import Data.Set(Set,empty,fromList,insert,member,size)

parse :: String -> [((Int,Int),(Int,Int))]
parse = p . words
  where
    p (a:"->":b:rest) = (read ("("++a++")"),read ("("++b++")")) : p rest
    p _ = []

horizontalOrVertical :: ((Int,Int),(Int,Int)) -> Bool
horizontalOrVertical ((x1,y1),(x2,y2)) = x1 == x2 || y1 == y2

points1 :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
points1 (a,b) = range (min a b,max a b)

countOverlapped :: [((Int,Int),(Int,Int))] -> Int
countOverlapped segments = size $ fromList $ scan empty $ concatMap points1 segments
  where
    scan seen [] = []
    scan seen (point:points)
      | member point seen = point : scan seen points
      | otherwise = scan (insert point seen) points

testData :: String
testData = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2\n"

test :: ()
test
  | (countOverlapped . filter horizontalOrVertical . parse) testData /= 5 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (countOverlapped . filter horizontalOrVertical . parse) $ readFile "input/05.txt"


points2 :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
points2 (a@(a1,a2),b@(b1,b2))
  | a1 == b1 || a2 == b2 = range (min a b,max a b)
  | (a1 < b1) == (a2 < b2) = zip (range (min a1 b1,max a1 b1)) (range (min a2 b2,max a2 b2))
  | otherwise = zip (reverse (range (min a1 b1,max a1 b1))) (range (min a2 b2,max a2 b2))

countOverlapped2 :: [((Int,Int),(Int,Int))] -> Int
countOverlapped2 segments = size $ fromList $ scan empty $ concatMap points2 segments
  where
    scan seen [] = []
    scan seen (point:points)
      | member point seen = point : scan seen points
      | otherwise = scan (insert point seen) points

test2 :: ()
test2
  | (countOverlapped2 . parse) testData /= 12 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (countOverlapped2 . parse) $ readFile "input/05.txt"
