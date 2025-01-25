module AOC201901 where

import AOC

aoc = AOC {
    day="../../2019/input/01",
    aocTests=[
        AOCTest {
            testData=unlines [
                "100756"
                ],
            testResult=Just "33583",
            testResult2=Just "50346"
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

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

result = sum . map fuel

fuel2 :: Int -> Int
fuel2 0 = 0
fuel2 mass = f + fuel2 f
  where f = max 0 (mass `div` 3 - 2)

result2 = sum . map fuel2
