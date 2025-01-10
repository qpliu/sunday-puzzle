module AOC202201 where

import Data.List(sort)

import AOC

aoc = AOC {
    day="../../2022/input/01",
    aocTests=[
        AOCTest {
            testData=unlines [
                "1000",
                "2000",
                "3000",
                "",
                "4000",
                "",
                "5000",
                "6000",
                "",
                "7000",
                "8000",
                "9000",
                "",
                "10000"
                ],
            testResult=Just "24000",
            testResult2=Just "45000"
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

parse :: String -> [[Int]]
parse = p . lines
  where
    p s | null this = []
        | otherwise = map read this : p rest
      where (this,rest) = span (not . null) $ dropWhile null s

result = maximum . map sum

result2 = sum . take 3 . reverse . sort . map sum
