{-
--- Day 3: Binary Diagnostic ---

The submarine has been making some odd creaking noises, so you ask it to
produce a diagnostic report just in case.

The diagnostic report (your puzzle input) consists of a list of binary numbers
which, when decoded properly, can tell you many useful things about the
conditions of the submarine. The first parameter to check is the power
consumption.

You need to use the binary numbers in the diagnostic report to generate two new
binary numbers (called the gamma rate and the epsilon rate). The power
consumption can then be found by multiplying the gamma rate by the epsilon
rate.

Each bit in the gamma rate can be determined by finding the most common bit in
the corresponding position of all numbers in the diagnostic report. For
example, given the following diagnostic report:

| 00100
| 11110
| 10110
| 10111
| 10101
| 01111
| 00111
| 11100
| 10000
| 11001
| 00010
| 01010

Considering only the first bit of each number, there are five 0 bits and seven
1 bits. Since the most common bit is 1, the first bit of the gamma rate is 1.

The most common second bit of the numbers in the diagnostic report is 0, so the
second bit of the gamma rate is 0.

The most common value of the third, fourth, and fifth bits are 1, 1, and 0,
respectively, and so the final three bits of the gamma rate are 110.

So, the gamma rate is the binary number 10110, or 22 in decimal.

The epsilon rate is calculated in a similar way; rather than use the most
common bit, the least common bit from each position is used. So, the epsilon
rate is 01001, or 9 in decimal. Multiplying the gamma rate (22) by the epsilon
rate (9) produces the power consumption, 198.

Use the binary numbers in your diagnostic report to calculate the gamma rate
and epsilon rate, then multiply them together. What is the power consumption
of the submarine? (Be sure to represent your answer in decimal, not binary.)
-}

import Data.Bits(setBit)

toBit :: Char -> Int
toBit '0' = 0
toBit _ = 1

collect :: [Int] -> (Int,[Int]) -> (Int,[Int])
collect bits (n,counts) = (n+1,zipWith (+) bits counts)

gammaEpsilon :: (Int,[Int]) -> (Int,Int)
gammaEpsilon (n,counts) = foldr addBit (0,0) $ zip [0..62] counts
  where
    addBit (bit,count) (gamma,epsilon)
      | 2*count > n = (setBit gamma bit,epsilon)
      | otherwise = (gamma,setBit epsilon bit)

testData :: String
testData = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010\n"

test :: ()
test
  -- blows up with empty input
  | (gammaEpsilon . foldr collect (0,repeat 0) . map (reverse . map toBit) . words) testData /= (22,9) = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (uncurry (*) . gammaEpsilon . foldr collect (0,repeat 0) . map (reverse . map toBit) . words) $ readFile "input/03.txt"

oxygenCO2 :: [[Int]] -> (Int,Int)
oxygenCO2 report = (oxygen 0 report,co2 0 report)

oxygen :: Int -> [[Int]] -> Int
oxygen bit nums
  | length nums == 1 =
      sum $ zipWith (*) (reverse (head nums)) [2^n | n <- [0..62]]
  | 2*sum (map (head . drop bit) nums) >= length nums =
      oxygen (bit+1) (filter ((== 1) . head . drop bit) nums)
   | otherwise =
      oxygen (bit+1) (filter ((== 0) . head . drop bit) nums)

co2 :: Int -> [[Int]] -> Int
co2 bit nums
  | length nums == 1 =
      sum $ zipWith (*) (reverse (head nums)) [2^n | n <- [0..62]]
  | 2*sum (map (head . drop bit) nums) >= length nums =
      co2 (bit+1) (filter ((== 0) . head . drop bit) nums)
   | otherwise =
      co2 (bit+1) (filter ((== 1) . head . drop bit) nums)

test2 :: ()
test2
  | (oxygenCO2 . map (map toBit) . words) testData /= (23,10) = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (uncurry (*) . oxygenCO2 . map (map toBit) . words) $ readFile "input/03.txt"
