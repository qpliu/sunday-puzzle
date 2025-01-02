module AOC202306 where

import Data.Char(isDigit)

import AOC

aoc = AOC {
    day="../../2023/input/06",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Time:      7  15   30",
                "Distance:  9  40  200"
                ],
            testResult=Just "288",
            testResult2=Just "71503"
            }
        ],
    aocCode=Code {
        codeParse=parse id,
        codeParse2=parse (filter isDigit),
        codeTest=result,
        codeTest2=result,
        codeResult=result,
        codeResult2=result
        }
    }

parse f = p . map (parseInts . f) . lines
  where p [time,distance] = zip time distance

ways (time,dist) = t2-t1+1
  where
    t1 = floor ((fromIntegral time-sqrt(disc))/2) + 1
    t2 = ceiling ((fromIntegral time+sqrt(disc))/2) - 1
    disc = fromIntegral (time^2-4*dist)

result = product . map ways
