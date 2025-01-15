module AOC202105 where

import Data.Map(Map,fromList,unionsWith,size)
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2021/input/05",
    aocTests=[
        AOCTest {
            testData=unlines [
                "0,9 -> 5,9",
                "8,0 -> 0,8",
                "9,4 -> 3,4",
                "2,2 -> 2,1",
                "7,0 -> 7,4",
                "6,4 -> 2,0",
                "0,9 -> 2,9",
                "3,4 -> 1,4",
                "0,0 -> 8,8",
                "5,5 -> 8,2"
                ],
            testResult=Just "5",
            testResult2=Just "12"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse2,
        pcodeTest=result,
        pcodeTest2=result,
        pcodeResult=result,
        pcodeResult2=result
        }
    }

parse = filter notDiagonal . map parseInts . lines
  where notDiagonal [x1,y1,x2,y2] = x1 == x2 || y1 == y2

mapVent :: [Int] -> Map (Int,Int) Int
mapVent [x1,y1,x2,y2]
  | x1 == x2 = fromList [((x1,y),1) | y <- [min y1 y2 .. max y1 y2]]
  | y1 == y2 = fromList [((x,y1),1) | x <- [min x1 x2 .. max x1 x2]]
  | signum (x1-x2) == signum (y1-y2) =
                fromList [((min x1 x2 + d,min y1 y2 + d),1)
                          | d <- [0 .. abs (x1-x2)]]
  | otherwise = fromList [((min x1 x2 + d,max y1 y2 - d),1)
                          | d <- [0 .. abs (x1-x2)]]

result ncpu =
    size . Data.Map.filter (> 1)
         . parallelMapReduce ncpu mapVent (unionsWith (+))

parse2 = map parseInts . lines
