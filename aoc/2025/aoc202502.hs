module AOC202502 where

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
        codeTest=result invalid,
        codeTest2=result invalid2,
        codeResult=result invalid,
        codeResult2=result invalid2
        }
    }

parse = map abs . parseInts

-- This is very slow, though not intolerably slow.
-- About 2 seconds for part 1 and 8 seconds for part 2 for my input on
-- my computer.

-- Something faster would be to take the first half of the first number
-- and the first half of the last number and iterate over the first half
-- of the number, then filter out numbers made from duplicating the
-- the two halves that are smaller than the first number or bigger than
-- the last number.  Also need to skip numbers with odd number of
-- digits, since they are all valid.
-- And some generalization for using the first 1/n of digits for part 2.

-- This could also be done using parallelism.

result :: (Int -> Bool) -> [Int] -> Int
result _ [] = 0
result test (a:b:rest) = result test rest + sum (filter test [a..b])

invalid a = b == c
  where (b,c) = splitAt (div (length (show a)) 2) (show a)

invalid2 a = any (test (show a)) [1 .. div (length (show a)) 2]
  where
    test a n = test2 (take n a) n (drop n a)
    test2 pre n a
      | pre == a = True
      | take n a /= pre = False
      | otherwise = test2 pre n (drop n a)
