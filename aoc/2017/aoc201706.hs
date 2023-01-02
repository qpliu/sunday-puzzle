{-
--- Day 6: Memory Reallocation ---

A debugger program here is having an issue: it is trying to repair a memory
reallocation routine, but it keeps getting stuck in an infinite loop.

In this area, there are sixteen memory banks; each memory bank can hold any
number of blocks. The goal of the reallocation routine is to balance the blocks
between the memory banks.

The reallocation routine operates in cycles. In each cycle, it finds the memory
bank with the most blocks (ties won by the lowest-numbered memory bank) and
redistributes those blocks among the banks. To do this, it removes all of the
blocks from the selected bank, then moves to the next (by index) memory bank
and inserts one of the blocks. It continues doing this until it runs out of
blocks; if it reaches the last memory bank, it wraps around to the first one.

The debugger would like to know how many redistributions can be done before a
blocks-in-banks configuration is produced that has been seen before.

For example, imagine a scenario with only four memory banks:

 - The banks start with 0, 2, 7, and 0 blocks. The third bank has the most
   blocks, so it is chosen for redistribution.
 - Starting with the next bank (the fourth bank) and then continuing to the
   first bank, the second bank, and so on, the 7 blocks are spread out over the
   memory banks. The fourth, first, and second banks get two blocks each, and
   the third bank gets one back. The final result looks like this: 2 4 1 2.
 - Next, the second bank is chosen because it contains the most blocks (four).
   Because there are four memory banks, each gets one block. The result is: 3 1
   2 3.
 - Now, there is a tie between the first and fourth memory banks, both of which
   have three blocks. The first bank wins the tie, and its three blocks are
   distributed evenly over the other three banks, leaving it with none: 0 2 3
   4.
 - The fourth bank is chosen, and its four blocks are distributed such that
   each of the four banks receives one: 1 3 4 1.
 - The third bank is chosen, and the same thing happens: 2 4 1 2.

At this point, we've reached a state we've seen before: 2 4 1 2 was already
seen. The infinite loop is detected after the fifth block redistribution cycle,
and so the answer in this example is 5.

Given the initial block counts in your puzzle input, how many redistribution
cycles must be completed before a configuration is produced that has been seen
before?
-}

import Data.Map(Map)
import qualified Data.Map
import Data.Set(Set,empty,insert,member)

redistribute :: [Int] -> [Int]
redistribute banks = map update $ zip [0..] banks
  where
    len = length banks
    (n,negativeIndex) = maximum $ zip banks [0,-1..]
    index = -negativeIndex
    update (i,count)
      | i == index = n `div` len
      | (i - index) `mod` len <= n `mod` len = count + n `div` len + 1
      | otherwise = count + n `div` len

countSteps :: Int -> Set [Int] -> [Int] -> Int
countSteps nsteps seen banks
  | banks `member` seen = nsteps
  | otherwise = countSteps (nsteps+1) (insert banks seen) (redistribute banks)

test
  | redistribute [0,2,7,0] /= [2,4,1,2] = error "a"
  | redistribute [2,4,1,2] /= [3,1,2,3] = error "b"
  | redistribute [3,1,2,3] /= [0,2,3,4] = error "c"
  | redistribute [0,2,3,4] /= [1,3,4,1] = error "d"
  | redistribute [1,3,4,1] /= [2,4,1,2] = error "e"
  | countSteps 0 empty [0,2,7,0] /= 5 = error "f"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (countSteps 0 empty . map read . words) $ readFile "input/06.txt"

countSteps2 :: Int -> Map [Int] Int -> [Int] -> (Int,Int)
countSteps2 nsteps seen banks =
    maybe next ((,) nsteps) (Data.Map.lookup banks seen)
  where
    next = countSteps2 (nsteps+1) (Data.Map.insert banks nsteps seen) (redistribute banks)

test2
  | countSteps2 0 Data.Map.empty [0,2,7,0] /= (5,1) = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (uncurry (-) . countSteps2 0 Data.Map.empty . map read . words) $ readFile "input/06.txt"
