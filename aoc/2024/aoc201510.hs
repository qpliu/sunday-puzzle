module AOC201510 where

import Data.Char(ord)

import AOC

aoc = AOC {
    day="../../2015/input/10",
    aocTests=[],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=undefined,
        codeTest2=undefined,
        codeResult=result 40,
        codeResult2=result 50
        }
    }

parse = map ((+ (-48)) . ord) . filter (/= '\n')

las :: [Int] -> [Int]
las (a:bs@(b:cs@(c:rest)))
  | a /= b = 1:a:las bs
  | a /= c = 2:a:las cs
  | otherwise = 3:a:las rest
las [a,b]
  | a == b = [2,a]
  | otherwise = [1,a,1,b]
las [a] = [1,a]

result n = length . head . drop n . iterate las
