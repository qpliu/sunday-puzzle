module AOC201805 where

import Data.Char(isAlpha,isLower,toLower)

import AOC

aoc = AOC {
    day="../../2018/input/05",
    aocTests=[
        AOCTest {
            testData = "dabAcCaCBAcCcaDA",
            testResult=Just "10",
            testResult2=Just "4"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=const result,
        pcodeTest2=result2,
        pcodeResult=const result,
        pcodeResult2=result2
        }
    }

parse = filter isAlpha

reduce :: String -> String -> Int
reduce reduced [] = length reduced
reduce [] (a:as) = reduce [a] as
reduce reduced@(r:rs) (a:as)
  | toLower r == toLower a && isLower r /= isLower a = reduce rs as
  | otherwise = reduce (a:reduced) as

result = reduce []

reduce2 :: String -> String -> Char -> Int
reduce2 reduced [] _ = length reduced
reduce2 [] (a:as) ch
  | toLower a == ch = reduce2 [] as ch
  | otherwise = reduce2 [a] as ch
reduce2 reduced@(r:rs) (a:as) ch
  | toLower a == ch = reduce2 reduced as ch
  | toLower r == toLower a && isLower r /= isLower a = reduce2 rs as ch
  | otherwise = reduce2 (a:reduced) as ch

result2 ncpu input =
    parallelMapReduce ncpu (reduce2 [] input) minimum ['a'..'z']
