module AOC202503 where

import Data.Char(ord)

import AOC

aoc = AOC {
    day="03",
    aocTests=[
        AOCTest {
            testData=unlines [
                "987654321111111",
                "811111111111119",
                "234234234234278",
                "818181911112111"
            ],
            testResult=Just "357",
            testResult2=Just "3121910778619"
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

parse = map (map toDigit) . lines
  where toDigit c = ord c - ord '0'

result = sum . map (largestJoltage2 2)

result2 = sum . map (largestJoltage2 12)

largestJoltage2 ndigits digits
  | ndigits == 0 = 0
  | otherwise = digit*10^(ndigits-1) + largestJoltage2 (ndigits-1) nextDigits
  where
    digit = maximum (take (length digits - ndigits + 1) digits)
    nextDigits = drop 1 $ dropWhile (/= digit) digits
