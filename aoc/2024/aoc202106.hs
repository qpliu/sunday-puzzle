module AOC202106 where

import Data.Map(adjust,elems,fromList)

import AOC

aoc = AOC {
    day="../../2021/input/06",
    aocTests=[
        AOCTest {
            testData=unlines [
                "3,4,3,1,2"
                ],
            testResult=Just "5934",
            testResult2=Just "26984457539"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 80,
        codeTest2=result 256,
        codeResult=result 80,
        codeResult2=result 256
        }
    }

parse =
    elems . foldr (adjust (+1)) (fromList $ zip [0..8] (repeat 0)) . parseInts

fish :: [Int] -> [Int]
fish [f0,f1,f2,f3,f4,f5,f6,f7,f8] = [f1,f2,f3,f4,f5,f6,f7+f0,f8,f0]

result n = sum . head . drop n . iterate fish
