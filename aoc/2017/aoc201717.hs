spinLock :: Int -> Int -> [Int] -> [Int]
spinLock limit skip list
  | n > limit = list
  | otherwise = spinLock limit skip (front ++ back ++ [n])
  where
    n = length list
    (back,front) = splitAt (skip `mod` n) list

test :: ()
test
  | take 3 (spinLock 2017 3 [0]) /= [638,1513,851] = error "a"
  | take 4 (reverse $ spinLock 2017 3 [0]) /= [2017,151,1134,1512] = error "b"
  | otherwise = ()

part1 :: Int -> Int
part1 skip = head (spinLock 2017 skip [0])

spinLock2 :: Int -> Int -> ((Int,Int),Int) -> ((Int,Int),Int)
spinLock2 limit skip ((n,index),valueAfter0)
  | n == limit = ((n,index),valueAfter0)
  | nextIndex == 1 = spinLock2 limit skip ((n+1,nextIndex),n+1)
  | otherwise = spinLock2 limit skip ((n+1,nextIndex),valueAfter0)
  where nextIndex = (index+skip) `mod` (n+1) + 1

test2 :: ()
test2
  | spinLock2 1 3 ((0,0),0) /= ((1,1),1) = error "a"
  | spinLock2 2 3 ((0,0),0) /= ((2,1),2) = error "b"
  | spinLock2 3 3 ((0,0),0) /= ((3,2),2) = error "c"
  | spinLock2 4 3 ((0,0),0) /= ((4,2),2) = error "d"
  | spinLock2 5 3 ((0,0),0) /= ((5,1),5) = error "e"
  | spinLock2 6 3 ((0,0),0) /= ((6,5),5) = error "f"
  | spinLock2 7 3 ((0,0),0) /= ((7,2),5) = error "g"
  | spinLock2 8 3 ((0,0),0) /= ((8,6),5) = error "h"
  | spinLock2 9 3 ((0,0),0) /= ((9,1),9) = error "i"
  | otherwise = ()

part2 :: Int -> Int
part2 skip = snd $ spinLock2 50000000 skip ((0,0),0)
