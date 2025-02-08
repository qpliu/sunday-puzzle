module AOC201502 where

import Data.List(sort)

import AOC

aoc = AOC {
    day="../../2015/input/02",
    aocTests=[
        AOCTest {
            testData=unlines [
                "2x3x4"
                ],
            testResult=Just "58",
            testResult2=Just "34"
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

parse = map (sort . parseInts) . lines

paper :: [Int] -> Int
paper [a,b,c] = 3*a*b + 2*b*c + 2*a*c

result = sum . map paper

ribbon :: [Int] -> Int
ribbon [a,b,c] = 2*(a+b) + a*b*c

result2 = sum . map ribbon
