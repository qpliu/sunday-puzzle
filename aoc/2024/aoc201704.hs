module AOC201704 where

import Data.List(nub,sort)
import Data.Set(empty,insert,member)

import AOC

aoc = AOC {
    day="../../2017/input/04",
    aocTests=[
        AOCTest {
            testData=unlines [
                "aa bb cc dd ee",
                "aa bb cc dd aa",
                "aa bb cc dd aaa"
                ],
            testResult=Just "2",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "abcde fghij",
                "abcde xyz ecdab",
                "a ab abc abd abf abj",
                "iiii oiii ooii oooi oooo",
                "oiii ioii iioi iiio"
                ],
            testResult=Nothing,
            testResult2=Just "3"
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

parse = map words . lines

result = length . filter valid
  where valid ws = length ws == length (nub ws)

result2 = length . filter (valid empty)
  where
    valid set [] = True
    valid set (w:ws)
      | member sorted set = False
      | otherwise = valid (insert sorted set) ws
      where sorted = sort w
