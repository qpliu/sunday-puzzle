{-
--- Day 1: Report Repair ---

After saving Christmas five years in a row, you've decided to take a vacation
at a nice resort on a tropical island. Surely, Christmas will go on without
you.

The tropical island has its own currency and is entirely cash-only. The gold
coins used there have a little picture of a starfish; the locals just call them
stars. None of the currency exchanges seem to have heard of them, but somehow,
you'll need to find fifty of these coins by the time you arrive so you can pay
the deposit on your room.

To save your vacation, you need to get all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each
day in the Advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

Before you leave, the Elves in accounting just need you to fix your expense
report (your puzzle input); apparently, something isn't quite adding up.

Specifically, they need you to find the two entries that sum to 2020 and then
multiply those two numbers together.

For example, suppose your expense report contained the following:

| 1721
| 979
| 366
| 299
| 675
| 1456

In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying
them together produces 1721 * 299 = 514579, so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to
2020; what do you get if you multiply them together?
-}

import Data.Set(Set,empty,fromList,insert,member)

scan :: [Int] -> [Int]
scan = fst . foldr collect ([],empty)
  where
    collect :: Int -> ([Int],Set Int) -> ([Int],Set Int)
    collect n (results,products)
      | (n*(2020-n)) `member` products = (n*(2020-n):results,products)
      | otherwise = (results,insert (n*(2020-n)) products)

test :: ()
test
  | scan [1721, 979, 366, 299, 675, 1456] /= [514579] = error "a"
  | otherwise = ()

part1 :: IO [Int]
part1 = fmap (scan . map read . words) $ readFile "input/01.txt"

scan2 :: [Int] -> [Int]
scan2 nums = [a*b*(2020-a-b) | a <- nums, b <- nums, b >= a, (2020-a-b) >= b , (2020-a-b) `member` set]
  where
    set = fromList nums

test2 :: ()
test2
  | scan2 [1721, 979, 366, 299, 675, 1456] /= [241861950] = error "a"
  | otherwise = ()

part2 :: IO [Int]
part2 = fmap (scan2 . map read . words) $ readFile "input/01.txt"
