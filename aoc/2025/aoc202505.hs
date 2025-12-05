module AOC202505 where

import Data.List(sort)

import AOC

aoc = AOC {
    day="05",
    aocTests=[
        AOCTest {
            testData=unlines [
                "3-5",
                "10-14",
                "16-20",
                "12-18",
                "",
                "1",
                "5",
                "8",
                "11",
                "17",
                "32"
            ],
            testResult=Just "3",
            testResult2=Just "14"
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

parse = toRanges [] . parseInts
  where
    toRanges ranges ids@(a:b:rest)
      | b < 0 = toRanges ((a,-b):ranges) rest
      | otherwise = (ranges,ids)

result (ranges,ids) = length $ filter fresh ids
  where fresh i = any (\ (a,b) -> a <= i && i <= b) ranges

merge [] = []
merge [a] = [a]
merge ((a,b):(c,d):rest)
  | b+1 < c = (a,b):merge ((c,d):rest)
  | otherwise = merge ((a,max b d):rest)

result2 = sum . map ((1 -) . uncurry (-)) . merge . sort . fst
