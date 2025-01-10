module AOC202203 where

import Data.Char(ord)
import Data.Set(elems,fromList,intersection)

import AOC

aoc = AOC {
    day="../../2022/input/03",
    aocTests=[
        AOCTest {
            testData=unlines [
                "vJrwpWtwJgWrhcsFMMfFFhFp",
                "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
                "PmmdzqPrVvPwwTWBwg",
                "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
                "ttgJtRGJQctTZtZT",
                "CrZsJsPPZsGzwwsLwLmpwMDw"
                ],
            testResult=Just "157",
            testResult2=Just "70"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse = map p . lines
  where p l = splitAt (length l `div` 2) l

priority :: Char -> Int
priority ch
  | ch >= 'a' && ch <= 'z' = ord ch - ord 'a' + 1
  | ch >= 'A' && ch <= 'Z' = ord ch - ord 'A' + 27

common :: (String,String) -> Char
common (a,b) = minimum $ fromList a `intersection` fromList b

result = sum . map (priority . common)

parse2 = p . lines
  where p l | null this = [] | otherwise = this : p rest
          where (this,rest) = splitAt 3 l

badge :: [String] -> Char
badge [a,b,c] =
    minimum $ fromList a `intersection` fromList b `intersection` fromList c

result2 = sum . map (priority . badge)
