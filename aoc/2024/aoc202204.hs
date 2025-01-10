module AOC202204 where

import AOC

aoc = AOC {
    day="../../2022/input/04",
    aocTests=[
        AOCTest {
            testData=unlines [
                "2-4,6-8",
                "2-3,4-5",
                "5-7,7-9",
                "2-8,3-7",
                "6-6,4-6",
                "2-6,4-8"
                ],
            testResult=Just "2",
            testResult2=Just "4"
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

parse = map (map abs . parseInts) . lines

fullyContained :: [Int] -> Bool
fullyContained [lo1,hi1,lo2,hi2] =
    (lo1 >= lo2 && hi1 <= hi2) || (lo2 >= lo1 && hi2 <= hi1)

result = length . filter fullyContained

overlaps :: [Int] -> Bool
overlaps [lo1,hi1,lo2,hi2] = hi1 >= lo2 && lo1 <= hi2

result2 = length . filter overlaps
