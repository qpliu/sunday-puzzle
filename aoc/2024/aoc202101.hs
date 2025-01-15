module AOC202101 where

import AOC

aoc = AOC {
    day="../../2021/input/01",
    aocTests=[
        AOCTest {
            testData=unlines [
                "199",
                "200",
                "208",
                "210",
                "200",
                "207",
                "240",
                "269",
                "260",
                "263"
                ],
            testResult=Just "7",
            testResult2=Just "5"
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

result measurements =
    length $ filter id $ zipWith (<) measurements $ drop 1 measurements

result2 measurements =
    length $ filter id $ zipWith (<) front back
  where
    middle = zipWith (+) (drop 1 measurements) (drop 2 measurements)
    front = zipWith (+) measurements middle
    back = zipWith (+) (drop 3 measurements) middle
