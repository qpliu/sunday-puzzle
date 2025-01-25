module AOC201904 where

import AOC

aoc = AOC {
    day="../../2019/input/04",
    aocTests=[],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

parse = map abs . parseInts

check :: Int -> Int
check n = ok (n `div` 10) (n `mod` 10) False
  where
    ok 0 _ hasDupe
      | hasDupe = 1
      | otherwise = 0
    ok n digit hasDupe
      | nextDigit > digit = 0
      | otherwise = ok (n `div` 10) nextDigit (hasDupe || digit == nextDigit)
      where nextDigit = n `mod` 10

result ncpu [lo,hi] = parallelMapReduce ncpu check sum [lo..hi]

check2 :: Int -> Int
check2 n = ok (n `div` 10) (n `mod` 10) False 0
  where
    ok 0 _ hasDupe dupeCount
      | hasDupe || dupeCount == 1 = 1
      | otherwise = 0
    ok n digit hasDupe dupeCount
      | nextDigit > digit = 0
      | nextDigit == digit = ok (n `div` 10) nextDigit hasDupe (dupeCount+1)
      | otherwise = ok (n `div` 10) nextDigit (hasDupe || dupeCount == 1) 0
      where nextDigit = n `mod` 10

result2 ncpu [lo,hi] = parallelMapReduce ncpu check2 sum [lo..hi]
