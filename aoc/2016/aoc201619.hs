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
