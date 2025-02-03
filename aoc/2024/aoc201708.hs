module AOC201708 where

import Data.Map(Map,empty,findWithDefault,insert)

import AOC

aoc = AOC {
    day="../../2017/input/08",
    aocTests=[
        AOCTest {
            testData=unlines [
                "b inc 5 if a > 1",
                "a inc 1 if b < 5",
                "c dec -10 if a >= 1",
                "c inc -20 if c == 10"
                ],
            testResult=Just "1",
            testResult2=Just "10"
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

execute :: (Map String Int,Int) -> [String] -> (Map String Int,Int)
execute (registers,highest) [r1,op1,v1,"if",r2,op2,v2]
  | cond = (insert r1 newVal registers,max highest newVal)
  | otherwise = (registers,highest)
  where
    newVal
      | op1 == "inc" = findWithDefault 0 r1 registers + read v1
      | op1 == "dec" = findWithDefault 0 r1 registers - read v1
    cond
      | op2 == ">" = findWithDefault 0 r2 registers > read v2
      | op2 == "<" = findWithDefault 0 r2 registers < read v2
      | op2 == ">=" = findWithDefault 0 r2 registers >= read v2
      | op2 == "<=" = findWithDefault 0 r2 registers <= read v2
      | op2 == "==" = findWithDefault 0 r2 registers == read v2
      | op2 == "!=" = findWithDefault 0 r2 registers /= read v2

parse = foldl execute (empty,0) . map words . lines

result = maximum . fst

result2 = snd
