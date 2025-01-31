module AOC201806 where

import Data.List(sort)
import Data.Map(Map,difference,fromList,singleton,unionsWith)

import AOC

aoc = AOC {
    day="../../2018/input/06",
    aocTests=[
        AOCTest {
            testData=unlines [
                "1, 1",
                "1, 6",
                "8, 3",
                "3, 4",
                "5, 5",
                "8, 9"
                ],
            testResult=Just "17",
            testResult2=Just "16"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result,
        pcodeTest2=result2 32,
        pcodeResult=result,
        pcodeResult2=result2 10000
        }
    }

parse = map parseInts . filter (not . null) . lines

dist :: [Int] -> [Int] -> Int
dist [x1,y1] [x2,y2] = abs (x1-x2) + abs (y1-y2)

bounds :: [[Int]] -> (Int,Int,Int,Int)
bounds coords = (head $ minimum coords, head $ minimum $ map tail coords,
                 head $ maximum coords, head $ maximum $ map tail coords)

closest :: [[Int]] -> [Int] -> [Int]
closest coords coord
  | d1 == d2 = []
  | otherwise = c1
  where
    ((d1,c1):(d2,_):_) = sort $ zip (map (dist coord) coords) coords

result ncpu coords = maximum $ difference table infinite
  where
    (xmin,ymin,xmax,ymax) = bounds coords
    infinite = fromList $ (([],()):) $ parallelMap ncpu makeInfinite
        ([[x,ymin-1] | x <- [xmin..xmax]]
         ++ [[x,ymax+1] | x <- [xmin..xmax]]
         ++ [[xmin-1,y] | y <- [ymin..ymax]]
         ++ [[xmax+1,y] | y <- [ymin..ymax]])
    makeInfinite coord = (closest coords coord,())

    table :: Map [Int] Int
    table = parallelMapReduce ncpu makeClosest (unionsWith (+))
        [[x,y] | x <- [xmin..xmax], y <- [ymin..ymax]]
    makeClosest coord = singleton (closest coords coord) 1

result2 limit ncpu coords =
    parallelMapReduce ncpu count sum [[x,y] | x <- [xmin-extent..xmax+extent],
                                              y <- [ymin-extent..ymax+extent]]
  where
    extent = 0 --limit `div` length coords
    (xmin,ymin,xmax,ymax) = bounds coords

    count :: [Int] -> Int
    count coord
      | sum (map (dist coord) coords) < limit = 1
      | otherwise = 0
