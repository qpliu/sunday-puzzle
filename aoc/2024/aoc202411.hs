module AOC202411 where

import Data.Map(alter,empty,toList)

import AOC

aoc = AOC {
    day="11",
    testData="125 17",
    testResult="55312",
    testData2="",
    testResult2="65601038650482",
    aocParse=parse,
    aocTest=result 25,
    aocResult=result 25,
    aocParse2=parse,
    aocTest2=result 75,
    aocResult2=result 75
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
  | n == 0 = Left 1
  | even ndigits = Right (n `div` tens, n `mod` tens)
  | otherwise = Left $ 2024*n
  where
    ndigits = log10 n
    tens = exp10 (ndigits `div` 2)

result nblinks =
    sum . map snd . head . drop nblinks . iterate step . flip zip (repeat 1)

step = toList . foldr collect empty

collect (n,count) counts = either collect1 collect2 $ blink n
  where
    collect1 n1 = alter (Just . maybe count (count+)) n1 counts
    collect2 (n1,n2) =
        alter (Just . maybe count (count+)) n1 $
        alter (Just . maybe count (count+)) n2 counts
