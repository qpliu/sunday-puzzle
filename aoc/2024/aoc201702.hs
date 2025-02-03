module AOC201702 where

import AOC

aoc = AOC {
    day="../../2017/input/02",
    aocTests=[
        AOCTest {
            testData=unlines [
                "5 1 9 5",
                "7 5 3",
                "2 4 6 8"
                ],
            testResult=Just "18",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "5 9 2 8",
                "9 4 7 3",
                "3 8 6 5"
                ],
            testResult=Nothing,
            testResult2=Just "9"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse = map parseInts . lines

chksum :: [Int] -> Int
chksum row = maximum row - minimum row

result = sum . map chksum

evenlyDivisibleValues :: [Int] -> Int
evenlyDivisibleValues row =
    sum [x `div` y | x <- row, y <- row, x /= y, x `mod` y == 0]

result2 = sum . map evenlyDivisibleValues
