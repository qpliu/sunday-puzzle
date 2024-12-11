module AOC202411 where

import Data.Map(Map,empty,insert,member,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="11",
    testData="125 17",
    testResult="55312",
    testData2="",
    testResult2="65601038650482",
    aocParse=parse,
    aocResult=result2 25,
    aocParse2=parse,
    aocResult2=result2 75
    }

parse :: String -> [Int]
parse = map read . words

log10 n = l 0 n
  where
    l i n
      | n == 0 = i
      | n < 10 = 1+i
      | n < 100 = 2+i
      | n < 1000 = 3+i
      | n < 10000 = 4+i
      | otherwise = l (i+5) (n`div`100000)

exp10 n = e 1 n
  where
    e f n
      | n == 0 = f
      | n == 1 = 10*f
      | n == 2 = 100*f
      | n == 3 = 1000*f
      | n == 4 = 10000*f
      | otherwise = e (100000*f) (n-5)

blink n
  | n == 0 = [1]
  | even ndigits = [n `div` tens, n `mod` tens]
  | otherwise = [2024*n]
  where
    ndigits = log10 n
    tens = exp10 (ndigits `div` 2)

result = length . head . drop 25 . iterate (concatMap blink)

blink2 n
  | n == 0 = Left 1
  | even ndigits = Right (n `div` tens, n `mod` tens)
  | otherwise = Left $ 2024*n
  where
    ndigits = log10 n
    tens = exp10 (ndigits `div` 2)

stoneCount k@(nblinks,n) (total,memo)
  | nblinks == 0 = (total+1,memo)
  | member k memo = (total+memo!k,memo)
  | otherwise = either count1 count2 $ blink2 n
  where
    count1 n = (total+count,insert k count newMemo)
      where (count,newMemo) = stoneCount (nblinks-1,n) (0,memo)
    count2 (n1,n2) = (total+count1+count2,insert k (count1+count2) newMemo2)
      where
        (count1,newMemo1) = stoneCount (nblinks-1,n1) (0,memo)
        (count2,newMemo2) = stoneCount (nblinks-1,n2) (0,newMemo1)
      
result2 nblinks = fst . foldr stoneCount (0,empty) . zip (repeat nblinks)
