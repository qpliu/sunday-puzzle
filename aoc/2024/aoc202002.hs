module AOC202002 where

import AOC

aoc = AOC {
    day="../../2020/input/02",
    aocTests=[
        AOCTest {
            testData=unlines [
                "1-3 a: abcde",
                "1-3 b: cdefg",
                "2-9 c: ccccccccc"
                ],
            testResult=Just "2",
            testResult2=Just "1"
        }
    ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

-- no password in my input contains spaces
parse = map (p . words) . lines
  where p [counts,[ch,':'],password] = (map abs $ parseInts counts,ch,password)

valid :: ([Int],Char,String) -> Int
valid ([n1,n2],ch,password)
  | n1 <= n && n <= n2 = 1
  | otherwise = 0
  where n = length $ filter (== ch) password

result ncpu = parallelMapReduce ncpu valid sum

valid2 :: ([Int],Char,String) -> Int
valid2 ([n1,n2],ch,password)
  | (password!!(n1-1) == ch) /= (password!!(n2-1) == ch) = 1
  | otherwise = 0

result2 ncpu = parallelMapReduce ncpu valid2 sum
