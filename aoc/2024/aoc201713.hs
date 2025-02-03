module AOC201713 where

import Data.Map(Map,alter,empty,toList)
import Data.Set(Set,insert,member,singleton)

import AOC

aoc = AOC {
    day="../../2017/input/13",
    aocTests=[
        AOCTest {
            testData=unlines [
                "0: 3",
                "1: 2",
                "4: 4",
                "6: 4"
                ],
            testResult=Just "24",
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

parse = map parseInts . lines

caught :: [Int] -> Bool
caught [depth,range] = depth `mod` (2*range-2) == 0

result = sum . map product . filter caught

-- period = 2*range-2
-- (delay + depth) `mod` period /= 0
-- delay `mod` period /= (-depth) `mod` period

makeFilters :: [[Int]] -> [(Int,Set Int)]
makeFilters = toList . foldr collect empty
  where
    collect [depth,range] =
        alter (Just . maybe (singleton bad) (insert bad)) period
      where
        period = 2*range-2
        bad = (-depth) `mod` period

filt :: (Int,Set Int) -> Int -> Bool
filt (period,bad) delay = not $ member (delay `mod` period) bad

sieve :: [Int] -> [(Int,Set Int)] -> [Int]
sieve ns [] = ns
sieve ns (f:fs) = sieve (filter (filt f) ns) fs

result2 = head . sieve [0..] . makeFilters
