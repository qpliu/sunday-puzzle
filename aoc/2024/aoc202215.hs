module AOC202215 where

import Data.Array(inRange)
import Data.List(sort)

import AOC

aoc = AOC {
    day="../../2022/input/15",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
                "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
                "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
                "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
                "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
                "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
                "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
                "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
                "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
                "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
                "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
                "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
                "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
                "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
                ],
            testResult=Just "26",
            testResult2=Just "56000011"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=const (result 10),
        pcodeTest2=result2 20,
        pcodeResult=const (result 2000000),
        pcodeResult2=result2 4000000
        }
    }

parse = map parseInts . lines

toRange :: Int -> [Int] -> [(Int,Int)]
toRange targetY [x,y,bx,by]
  | xDist < 0 = []
  | by == targetY && x < bx = [(x-xDist,bx-1)]
  | by == targetY && x > bx = [(bx+1,x+xDist)]
  | by == targetY && x == bx = []
  | otherwise = [(x-xDist,x+xDist)]
  where
    dist = abs (x-bx) + abs (y-by)
    xDist = dist - abs (y-targetY)

count :: Int -> [(Int,Int)] -> Int
count n [(xmin,xmax)] = n + xmax-xmin+1
count n ((x1,x2):(x3,x4):rest)
  | x2 + 1 < x3 = count (n + x2-x1+1) ((x3,x4):rest)
  | otherwise = count n ((x1,max x2 x4):rest)

result targetY = count 0 . sort . concatMap (toRange targetY)

ranges :: Int -> Int -> [[Int]] -> [(Int,Int)]
ranges maxXY index sensors =
    toRanges $ sort (0:maxXY:filter (inRange (1,maxXY-1))
                                    (map (head . drop index) sensors))
  where
    toRanges [_] = []
    toRanges (a:rest@(b:_)) = (a,b) : toRanges rest

-- Divide the search area into rectangular regions (x0,y0),(x1,y1) such
-- that there are no sensors at (x,y) where
--    x0 < x < x1 and (y < y0 or y > y1)
--  or
--    y0 < y < y1 and (x < x0 or x > x1)
regions :: Int -> [[Int]] -> [((Int,Int),(Int,Int))]
regions maxXY sensors = [(xrange,yrange) | xrange <- ranges maxXY 0 sensors,
                                           yrange <- ranges maxXY 1 sensors]

isolate :: [[Int]] -> ((Int,Int),(Int,Int)) -> [(Int,Int)]
isolate sensors ((x0,y0),(x1,y1))
  | xw == xe && yn == ys = [(xw,yn)]
  | otherwise = []
  where
    nws = filter ((<= x0) . head) $ filter ((<= y0) . head . tail) $ sensors
    nes = filter ((>= x1) . head) $ filter ((<= y0) . head . tail) $ sensors
    sws = filter ((<= x0) . head) $ filter ((>= y1) . head . tail) $ sensors
    ses = filter ((>= x1) . head) $ filter ((>= y1) . head . tail) $ sensors

    xnw | null nws = x0-1
        | otherwise = maximum $ map (xintercept y0 (+)) nws
    xne | null nes = x1+1
        | otherwise = minimum $ map (xintercept y0 (-)) nes
    xsw | null sws = x0-1
        | otherwise = maximum $ map (xintercept y1 (+)) sws
    xse | null ses = x1+1
        | otherwise= minimum $ map (xintercept y1 (-)) ses

    xintercept targetY plusorminus [x,y,bx,by] =
        x `plusorminus` (abs (x-bx) + abs (y-by) - abs (y-targetY))

    yn = y0 + (xnw - xne) `div` 2 + 1
    ys = y1 - (xsw - xse) `div` 2 - 1

    ynw = y0 + xnw - x0
    ysw = y1 - (xsw - x0)
    yne = y0 + x1 - xne
    yse = y1 - (x1 - xse)

    xw = x0 + (ynw - ysw) `div` 2 + 1
    xe = x1 - (yne - yse) `div` 2 - 1

result2 maxXY ncpu sensors = 4000000*x+y
  where ((x,y):_) = parallelMapReduce ncpu (isolate sensors) concat
                                      (regions maxXY sensors)
