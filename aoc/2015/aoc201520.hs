{-
--- Day 20: Infinite Elves and Infinite Houses ---

To keep the Elves busy, Santa has them deliver some presents by hand,
door-to-door. He sends them down a street with infinite houses numbered
sequentially: 1, 2, 3, 4, 5, and so on.

Each Elf is assigned a number, too, and delivers presents to houses based on
that number:

 - The first Elf (number 1) delivers presents to every house: 1, 2, 3, 4, 5,
   ....
 - The second Elf (number 2) delivers presents to every second house: 2, 4, 6,
   8, 10, ....
 - Elf number 3 delivers presents to every third house: 3, 6, 9, 12, 15, ....

There are infinitely many Elves, numbered starting with 1. Each Elf delivers
presents equal to ten times his or her number at each house.

So, the first nine houses on the street end up like this:

House 1 got 10 presents.
House 2 got 30 presents.
House 3 got 40 presents.
House 4 got 70 presents.
House 5 got 60 presents.
House 6 got 120 presents.
House 7 got 80 presents.
House 8 got 150 presents.
House 9 got 130 presents.

The first house gets 10 presents: it is visited only by Elf 1, which delivers
1 * 10 = 10 presents. The fourth house gets 70 presents, because it is visited
by Elves 1, 2, and 4, for a total of 10 + 20 + 40 = 70 presents.

What is the lowest house number of the house to get at least as many presents
as the number in your puzzle input?
-}
import Data.List(nub,subsequences)

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

factors :: Int -> [Int]
factors n = f (take 10 primes) n []
  where
    f [] _ _ = []
    f (p:ps) n facts
      | p > n = facts
      | n `mod` p /= 0 = f ps n facts
      | otherwise = f (p:ps) (n `div` p) (p:facts)

presents :: Int -> Int
presents house = 10*(sum $ map product $ nub $ subsequences $ factors house)

test :: ()
test
  | presents 1 /= 10 = error "a"
  | presents 2 /= 30 = error "a"
  | presents 3 /= 40 = error "a"
  | presents 4 /= 70 = error "a"
  | presents 5 /= 60 = error "a"
  | presents 6 /= 120 = error "a"
  | presents 7 /= 80 = error "a"
  | presents 8 /= 150 = error "a"
  | presents 9 /= 130 = error "a"
  | otherwise = ()

part1 :: Int -> Int
part1 input = head $ filter ((>= input) . presents) [1..]

part2presents :: Int -> Int
part2presents house = 11*(sum $ filter ((>= house) . (*50)) $ map product $ nub $ subsequences $ factors house)

part2 :: Int -> Int
part2 input = head $ filter ((>= input) . part2presents) [1..]
