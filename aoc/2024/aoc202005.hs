module AOC202005 where

import Data.List(sort)

import AOC

aoc = AOC {
    day="../../2020/input/05",
    aocTests=[],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse = map (foldl toNum 0 . map toDigit) . lines
  where
    toDigit 'B' = 1
    toDigit 'F' = 0
    toDigit 'L' = 0
    toDigit 'R' = 1
    toNum s digit = s*2+digit

result = maximum

result2 seats = snd $ maximum $ zip (zipWith (-) (tail sorted) sorted) sorted
  where sorted = sort seats
