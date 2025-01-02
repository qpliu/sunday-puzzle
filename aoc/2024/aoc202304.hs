module AOC202304 where

import Data.Bits(shiftL)
import Data.Set(fromList,intersection,size)

import AOC

aoc = AOC {
    day="../../2023/input/04",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
                "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
                "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
                "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
                "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
                "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
                ],
            testResult=Just "13",
            testResult2=Just "30"
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

parse = map (p . words) . lines
  where
    p numbers = (size (intersection (fromList winningNumbers) (fromList numbersYouHave)),1)
      where (winningNumbers,numbersYouHave) = span (/= "|") numbers

result :: [(Int,Int)] -> Int
result = sum . map (shiftL 1 . (flip (-) 1)) . filter (> 0) . map fst

result2 :: [(Int,Int)] -> Int
result2 [] = 0
result2 ((wins,count):rest) =
    count + result2 (map (fmap (count +)) copied ++ notCopied)
  where (copied,notCopied) = splitAt wins rest
