module AOC202502 where

import Data.Set(fromList)

import AOC

aoc = AOC {
    day="02",
    aocTests=[
        AOCTest {
            testData=unlines [
                "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
            ],
            testResult=Just "1227775554",
            testResult2=Just "4174379265"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result (invalids 2),
        codeTest2=result multiInvalids,
        codeResult=result (invalids 2),
        codeResult2=result multiInvalids
        }
    }

parse = toPairs . parseInts
  where
    toPairs [] = []
    toPairs (lo:hi:rest) = (lo,-hi):toPairs rest

countDigits n
  | n < 10 = 1
  | otherwise = 1 + countDigits (div n 10)

rangeStart n i
  | mod ndigits n /= 0 = 10^(div ndigits n)
  | otherwise = div i (10^(ndigits - div ndigits n))
  where ndigits = countDigits i

rangeEnd n i
  | mod ndigits n /= 0 = 10^(div ndigits n) - 1
  | otherwise = div i (10^(ndigits - div ndigits n))
  where ndigits = countDigits i

range n (a,b) = [rangeStart n a .. rangeEnd n b]

invalids :: Int -> (Int,Int) -> [Int]
invalids n (a,b) = [j | i <- range n (a,b), j <- [dup n i], j >= a, j <= b]

dup n i = d n
  where
    ndigits = countDigits i
    d n
      | n <= 1 = i
      | otherwise = i + 10^ndigits * d (n-1)

multiInvalids (a,b) = concat [invalids n (a,b) | n <- [2..countDigits b]]

result generateInvalids = sum . map (sum . fromList . generateInvalids)
