module AOC201517 where

import Data.List(group,sort,subsequences)

import AOC

aoc = AOC {
    day="../../2015/input/17",
    aocTests=[
        AOCTest {
            testData=unlines [
                "20 15 10 5 5"
                ],
            testResult=Just "4",
            testResult2=Just "3"
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=parseInts,
        codeTest=result 25,
        codeTest2=result2 25,
        codeResult=result 150,
        codeResult2=result2 150
        }
    }

combinations :: Int -> Int -> [Int] -> [Int]
combinations count 0 _ = [count]
combinations _ _ [] = []
combinations count total (n:ns)
  | total >= n =
      combinations (count+1) (total-n) ns ++ combinations count total ns
  | otherwise = combinations count total ns

result n = length . combinations 0 n

result2 n = length . head . group . sort . combinations 0 n
