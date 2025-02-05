module AOC201603 where

import AOC

aoc = AOC {
    day="../../2016/input/03",
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

parse = map parseInts . lines

possible :: [Int] -> Bool
possible [a,b,c] = a <= b + c && b <= a + c && c <= a + b

result = length . filter possible

countPossible :: [[Int]] -> [Int]
countPossible [] = []
countPossible ([a1,a2,a3]:[b1,b2,b3]:[c1,c2,c3]:rest) =
    (n1+n2+n3) : countPossible rest
  where
    n1 | a1 <= b1+c1 && b1 <= a1+c1 && c1 <= a1+b1 = 1 | otherwise = 0
    n2 | a2 <= b2+c2 && b2 <= a2+c2 && c2 <= a2+b2 = 1 | otherwise = 0
    n3 | a3 <= b3+c3 && b3 <= a3+c3 && c3 <= a3+b3 = 1 | otherwise = 0

result2 = sum . countPossible
