import Data.Map(Map,empty,findWithDefault,fromList,insert)

play :: [Int] -> [Int]
play = playStart 1 empty
  where
    playStart turn ages [] = error "playStart"
    playStart turn ages [n] = playNext turn ages n
    playStart turn ages (n:ns) = n : playStart (turn+1) (insert n turn ages) ns
    playNext turn ages n = n : playNext (turn+1) (insert n turn ages) (turn - findWithDefault turn n ages)

veSeq :: [Int] -> Int -> Int
veSeq start i
  | i <= length start = start !! (i-1)
  | otherwise = run (fromList (zip (init start) [1..])) (length start) (last start)
  where
    run history j n
      | j == i = n
      | otherwise = run (insert n j history) (j+1) next
      where next = j - findWithDefault j n history

test :: ()
test
  | play [0,3,6] !! 2019 /= 436 = error "a"
  | play [1,3,2] !! 2019 /= 1 = error "b"
  | play [2,1,3] !! 2019 /= 10 = error "c"
  | play [1,2,3] !! 2019 /= 27 = error "d"
  | play [2,3,1] !! 2019 /= 78 = error "e"
  | play [3,2,1] !! 2019 /= 438 = error "f"
  | play [3,1,2] !! 2019 /= 1836 = error "g"
  | otherwise = ()

part1 :: [Int] -> Int
part1 input = play input !! 2019

-- Get stack overflow for part 2.

-- https://oeis.org/A181391
-- No indication of any faster algorithms.
