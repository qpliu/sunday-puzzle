module AOC201524 where

import AOC

aoc = AOC {
    day="../../2015/input/24",
    aocTests=[
        AOCTest {
            testData="1 2 3 4 5 7 8 9 10 11",
            testResult=Just "99",
            testResult2=Just "44"
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=parseInts,
        codeTest=result 3,
        codeTest2=result 4,
        codeResult=result 3,
        codeResult2=result 4
        }
    }

metric :: [Int] -> (Int,Int)
metric packages = (length packages,product packages)

atTarget :: Int -> [Int] -> [[Int]]
atTarget 0 _ = [[]]
atTarget _ [] = []
atTarget target (p:ps)
  | target < p = atTarget target ps
  | otherwise =  atTarget target ps ++ map (p:) (atTarget (target-p) ps)

result n weights = snd $ minimum $ map metric $ atTarget target weights
  where target = sum weights `div` n
