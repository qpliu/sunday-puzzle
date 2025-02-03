module AOC201715 where

import Data.Bits((.&.))

import AOC

aoc = AOC {
    day="../../2017/input/15",
    aocTests=[
        AOCTest {
            testData=unlines [
                "generator A starts with 65",
                "generator B starts with 8921"
                ],
            testResult=Just "588",
            testResult2=Just "309"
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=parseInts,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

genA :: Int -> Int
genA v = (16807*v) `mod` 2147483647

genB :: Int -> Int
genB v = (48271*v) `mod` 2147483647

match :: (Int,Int) -> Bool
match (a,b) = (a .&. 65535) == (b .&. 65535)

result [a,b] =
    length $ filter match $ take 40000000
           $ zip (iterate genA a) (iterate genB b)

result2 [a,b] =
    length $ filter match $ take 5000000
           $ zip (filter ((== 0) . (`mod` 4)) $ iterate genA a)
                 (filter ((== 0) . (`mod` 8)) $ iterate genB b)
