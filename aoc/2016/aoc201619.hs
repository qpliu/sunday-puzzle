{-
--- Day 19: An Elephant Named Joseph ---

The Elves contact you over a highly secure emergency channel. Back at the North
Pole, the Elves are busy misunderstanding White Elephant parties.

Each Elf brings a present. They all sit in a circle, numbered starting with
position 1. Then, starting with the first Elf, they take turns stealing all the
presents from the Elf to their left. An Elf with no presents is removed from
the circle and does not take turns.

For example, with five Elves (numbered 1 to 5):

  1
5   2
 4 3

 - Elf 1 takes Elf 2's present.
 - Elf 2 has no presents and is skipped.
 - Elf 3 takes Elf 4's present.
 - Elf 4 has no presents and is also skipped.
 - Elf 5 takes Elf 1's two presents.
 - Neither Elf 1 nor Elf 2 have any presents, so both are skipped.
 - Elf 3 takes Elf 5's three presents.

So, with five Elves, the Elf that sits starting in position 3 gets all the
presents.

With the number of Elves given in your puzzle input, which Elf gets all the
presents?
-}

-- O(log n)
-- Returning the number of the Elf from whom the Elf getting all the
-- presents last takes from, since I suspect that'll be useful for part 2,
-- say to calculate how many presents are taken last.  Perhaps in part 2,
-- each Elf brings the Elf's number number of presents instead of one.

elephant :: Int -> Int -> Int -> (Int,Int)
elephant first interval count
  | count < 2 = (first,first) -- should not happen
  | count == 2 = (first,first+interval) -- first has everything brought by first through first+interval-1, takes the remaining from first+interval
  | count == 3 = (first+2*interval,first) -- first+2*interval takes everything brought by first through first+2*interval-1 from first, already has the remaining
  | even count = elephant first (2*interval) (count `div` 2)
  | otherwise = elephant (first+2*interval) (2*interval) (count `div` 2)

test :: ()
test
  | elephant 1 1 5 /= (3,5) = error "a"
  | otherwise = ()

part1 :: Int -> Int
part1 n = fst $ elephant 1 1 n

-- Brute-force some smaller part2 examples to gain intuition.
steal :: [a] -> (a,[a])
steal elves = (head (drop n elves),tail (take n elves) ++ tail (drop n elves) ++ [head elves])
  where n = length elves `div` 2

-- O(n)
-- If with n-1 Elves, Elf m gets everything,
-- then, with n Elves, Elf 1 takes from Elf floor(n/2+1), then
-- move Elf 1 to the end of the line of n-1 Elves, so if m = n-1,
-- Elf 1 gets everything.  If m+1 < floor(n/2+1) Elf m+1 gets everything.
-- Otherwise, Elf m+2 gets everything.

part2 :: Int -> Int
part2 n
  | n <= 2 = 1
  | m == n-1 = 1
  | m < eliminated-1 = m+1
  | otherwise = m+2
  where
    m = part2 (n-1)
    eliminated = n `div` 2 + 1
