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
  where toDigit = (+ (0 - ord '0')) . ord

result = sum . map largestJoltage

largestJoltage [] = 0
largestJoltage [_] = 0
largestJoltage (a:as) = max (largest1 a as) (largestJoltage as)
  where largest1 a = maximum . map (+ (10 * a))

result2 = sum . map (largestJoltage2 12)

largestJoltage2 ndigits digits
  | ndigits == 0 = 0
  | otherwise = digit*10^(ndigits-1) + largestJoltage2 (ndigits-1) rest
  where
    digit = maximum (take (length digits - ndigits + 1) digits)
    rest = drop 1 $ dropWhile (/= digit) digits
