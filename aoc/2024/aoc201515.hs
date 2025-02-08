module AOC201515 where

import AOC

aoc = AOC {
    day="../../2015/input/15",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8",
                "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"
                ],
            testResult=Just "62842880",
            testResult2=Just "57600000"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

parse = map parseInts . lines

combinations :: Int -> [[Int]] -> [[Int]]
combinations n [ingredient] = [map (n*) ingredient]
combinations n (ingredient:ingredients) =
    [zipWith (+) (map (i*) ingredient) combo
     | i <- [0 .. n],
       combo <- combinations (n-i) ingredients]

score :: [Int] -> Int
score = product . map (max 0) . init

result ncpu = parallelMapReduce ncpu score maximum . combinations 100

result2 ncpu = parallelMapReduce ncpu score maximum . filter ((== 500) . last)
                                                    . combinations 100
