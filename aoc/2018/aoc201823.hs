{-
--- Day 23: Experimental Emergency Teleportation ---

Using your torch to search the darkness of the rocky cavern, you finally locate
the man's friend: a small reindeer.

You're not sure how it got so far in this cave. It looks sick - too sick to
walk - and too heavy for you to carry all the way back. Sleighs won't be
invented for another 1500 years, of course.

The only option is experimental emergency teleportation.

You hit the "experimental emergency teleportation" button on the device and
push I accept the risk on no fewer than 18 different warning messages.
Immediately, the device deploys hundreds of tiny nanobots which fly around the
cavern, apparently assembling themselves into a very specific formation. The
device lists the X,Y,Z position (pos) for each nanobot as well as its signal
radius (r) on its tiny screen (your puzzle input).

Each nanobot can transmit signals to any integer coordinate which is a distance
away from it less than or equal to its signal radius (as measured by Manhattan
distance). Coordinates a distance away of less than or equal to a nanobot's
signal radius are said to be in range of that nanobot.

Before you start the teleportation process, you should determine which nanobot
is the strongest (that is, which has the largest signal radius) and then, for
that nanobot, the total number of nanobots that are in range of it, including
itself.

For example, given the following nanobots:

| pos=<0,0,0>, r=4
| pos=<1,0,0>, r=1
| pos=<4,0,0>, r=3
| pos=<0,2,0>, r=1
| pos=<0,5,0>, r=3
| pos=<0,0,3>, r=1
| pos=<1,1,1>, r=1
| pos=<1,1,2>, r=1
| pos=<1,3,1>, r=1

The strongest nanobot is the first one (position 0,0,0) because its signal
radius, 4 is the largest. Using that nanobot's location and signal radius, the
following nanobots are in or out of range:

 - The nanobot at 0,0,0 is distance 0 away, and so it is in range.
 - The nanobot at 1,0,0 is distance 1 away, and so it is in range.
 - The nanobot at 4,0,0 is distance 4 away, and so it is in range.
 - The nanobot at 0,2,0 is distance 2 away, and so it is in range.
 - The nanobot at 0,5,0 is distance 5 away, and so it is not in range.
 - The nanobot at 0,0,3 is distance 3 away, and so it is in range.
 - The nanobot at 1,1,1 is distance 3 away, and so it is in range.
 - The nanobot at 1,1,2 is distance 4 away, and so it is in range.
 - The nanobot at 1,3,1 is distance 5 away, and so it is not in range.

In this example, in total, 7 nanobots are in range of the nanobot with the
largest signal radius.

Find the nanobot with the largest signal radius. How many nanobots are in range
of its signals?
-}

import Data.Char(isDigit)

parse :: String -> [(Int,Int,Int,Int)]
parse = map p . lines
  where
    p s = (read r,read x,read y,read z)
      where
        (x,s1) = span isNum $ dropWhile (not . isNum) s
        (y,s2) = span isNum $ dropWhile (not . isNum) s1
        (z,s3) = span isNum $ dropWhile (not . isNum) s2
        r = filter isNum s3
    isNum c = isDigit c || c == '-'

inRange :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> Bool
inRange (r1,x1,y1,z1) (r2,x2,y2,z2) =
    r1 >= abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

getCount :: [(Int,Int,Int,Int)] -> Int
getCount bots = length $ filter (inRange (maximum bots)) bots

testData :: String
testData = "pos=<0,0,0>, r=4\npos=<1,0,0>, r=1\npos=<4,0,0>, r=3\npos=<0,2,0>, r=1\npos=<0,5,0>, r=3\npos=<0,0,3>, r=1\npos=<1,1,1>, r=1\npos=<1,1,2>, r=1\npos=<1,3,1>, r=1"

test :: ()
test
  | getCount (parse testData) /= 7 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (getCount . parse) $ readFile "input/23.txt"

{-
--- Part Two ---

Now, you just need to figure out where to position yourself so that you're
actually teleported when the nanobots activate.

To increase the probability of success, you need to find the coordinate which
puts you in range of the largest number of nanobots. If there are multiple,
choose one closest to your position (0,0,0, measured by manhattan distance).

For example, given the following nanobot formation:

| pos=<10,12,12>, r=2
| pos=<12,14,12>, r=2
| pos=<16,12,12>, r=4
| pos=<14,14,14>, r=6
| pos=<50,50,50>, r=200
| pos=<10,10,10>, r=5

Many coordinates are in range of some of the nanobots in this formation.
However, only the coordinate 12,12,12 is in range of the most nanobots: it is
in range of the first five, but is not in range of the nanobot at 10,10,10.
(All other coordinates are in range of fewer than five nanobots.) This
coordinate's distance from 0,0,0 is 36.

Find the coordinates that are in range of the largest number of nanobots. What
is the shortest manhattan distance between any of those points and 0,0,0?
-}

testData2 :: String
testData2 = "pos=<10,12,12>, r=2\npos=<12,14,12>, r=2\npos=<16,12,12>, r=4\npos=<14,14,14>, r=6\npos=<50,50,50>, r=200\npos=<10,10,10>, r=5"

-- First, get a rough idea of where the answer might be.
rescale :: Int -> [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
rescale factor = map divide
  where divide (r,x,y,z) = (r `div` factor,x `div` factor,y `div` factor,z `div` factor)

bruteForce :: Int -> Int -> (Int,Int,Int) -> [(Int,Int,Int,Int)] -> [(Int,(Int,(Int,Int,Int)))]
bruteForce maxDist limit (x0,y0,z0) bots = [(0 - length (filter (`inRange` (0,x,y,z)) bots),(abs x + abs y + abs z,(x,y,z))) | x <- [x0-limit..x0+limit], y <- [y0-limit..y0+limit], z <- [z0-limit..z0+limit], abs x + abs y + abs z < maxDist]

-- Scaling out my input data, the largest number of bots in range is 879.
-- The minimum distance from (0,0,0) is 95.5M - 95.7M.

-- Zooming in, the largest number of bots is 894.
-- The minimum distance is 95.5409M - 95.5411M.

-- Zooming in more, 907 bots, minimum distance 95541.00k - 95541.02k.

-- Fully zoomed, 910 bots, minimum distance 95541011.

-- This code keeps going down the wrong path.
search :: (Int,Int,[(Int,Int,Int,Int)]) -> (Int,Int,(Int,(Int,Int,Int))) -> (Int,Int,(Int,(Int,Int,Int)))
search (f,limit,bots) (nbots,scaleFactor,(dist,(x,y,z))) = (-negBots,sf,(newDist*sf,(newx*sf,newy*sf,newz*sf)))
  where
    sf = scaleFactor `div` f
    (negBots,(newDist,(newx,newy,newz))) = minimum $ bruteForce (dist`div`sf+2) limit (x`div`sf,y`div`sf,z`div`sf) (rescale sf bots)
