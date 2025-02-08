module AOC201525 where

import AOC

aoc = AOC {
    day="../../2015/input/25",
    aocTests=[
        AOCTest {
            testData="5 6",
            testResult=Just "31663883",
            testResult2=Nothing
            },
        AOCTest {
            testData="6 5",
            testResult=Just "1534922",
            testResult2=Nothing
            },
        AOCTest {
            testData="6 6",
            testResult=Just "27995004",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=const (),
        codeResult=result,
        codeResult2=const ()
        }
    }

index :: [Int] -> Int
index [row,column] = (baseRow*(baseRow-1)) `div` 2 + column - 1
  where baseRow = row+column-1

parse = index . parseInts

generate :: Int -> Int -> Int
generate seed count = g 0 seed
  where
    g i n
      | i == count = n
      | otherwise = g (i+1) $! ((n*252533) `mod` 33554393)
              -- the $! prevents stack overflow

result = generate 20151125
