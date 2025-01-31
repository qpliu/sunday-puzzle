module AOC201825 where

import Data.List(partition)

import AOC

aoc = AOC {
    day="../../2018/input/25",
    aocTests=[
        AOCTest {
            testData=unlines [
                " 0,0,0,0",
                " 3,0,0,0",
                " 0,3,0,0",
                " 0,0,3,0",
                " 0,0,0,3",
                " 0,0,0,6",
                " 9,0,0,0",
                "12,0,0,0"
                ],
            testResult=Just "2",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "-1,2,2,0",
                "0,0,2,-2",
                "0,0,0,-2",
                "-1,2,0,0",
                "-2,-2,-2,2",
                "3,0,2,-1",
                "-1,3,2,2",
                "-1,0,-1,0",
                "0,2,1,-2",
                "3,0,0,0"
                ],
            testResult=Just "4",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "1,-1,0,1",
                "2,0,-1,0",
                "3,2,-1,0",
                "0,0,3,1",
                "0,0,-1,-1",
                "2,3,-2,0",
                "-2,2,0,0",
                "2,-2,0,-1",
                "1,-1,0,-1",
                "3,2,0,2"
                ],
            testResult=Just "3",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "1,-1,-1,-2",
                "-2,-2,0,1",
                "0,2,1,3",
                "-2,3,-2,1",
                "0,2,3,-2",
                "-1,-1,1,-2",
                "0,-2,-1,0",
                "-2,2,3,-1",
                "1,2,2,0",
                "-1,-2,0,-2"
                ],
            testResult=Just "8",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=map parseInts . lines,
        codeParse2=const (),
        codeTest=result,
        codeTest2=undefined,
        codeResult=result,
        codeResult2=const ()
        }
    }

joins :: [Int] -> [Int] -> Bool
joins [x1,y1,z1,t1] [x2,y2,z2,t2] =
    abs (x1-x2) + abs (y1-y2) + abs (z1-z2) + abs (t1-t2) <= 3

join :: [Int] -> [[[Int]]] -> [[[Int]]]
join point constellations = (point:concat joined):distant
  where (joined,distant) = partition (any (joins point)) constellations

result = length . foldr join []
