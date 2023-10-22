import Data.Map(Map,fromList,(!))

escape :: Int -> Int -> Rational
escape lead lights
  | lights < lead = 1
  | lead <= 1 = 1/2*escape lead (lights-1) + 1/4*escape (lead+1) (lights-1)
  | otherwise = 1/2*escape lead (lights-1) + 1/4*escape (lead+1) (lights-1) + 1/4*escape (lead-1) (lights-1)

escape2 :: Int -> Int -> Rational
escape2 lead lights = memo!(lead,lights)
  where
    memo = fromList [((le,li),esc le li) | le <- [1..lights+1], li <- [0..lights]]
    esc le li
      | li < le = 1
      | le <= 1 = memo!(le,li-1)/2 + memo!(le+1,li-1)/4
      | otherwise = memo!(le,li-1)/2 + memo!(le+1,li-1)/4 + memo!(le-1,li-1)/4

caught :: Int -> Int -> Rational
caught lead time
  | lead <= 0 && time == 0 = 1
  | lead <= 0 = 0
  | lead > time = 0
  | otherwise = caught lead (time-1)/2 + caught (lead+1) (time-1)/4 + caught (lead-1) (time-1)/4

caught2 :: Int -> Int -> Rational
caught2 lead time = memo!(lead,time)
  where
    memo = fromList [((l,t),ca l t) | l <- [0..lead+time+1], t <- [0..time]]
    ca l t
      | l <= 0 && t == 0 = 1
      | l <= 0 = 0
      | l > t = 0
      | otherwise = memo!(l,t-1)/2 + memo!(l+1,t-1)/4 + memo!(l-1,t-1)/4

catalan :: Integer -> Integer
catalan n = product [n+2..2*n] `div` product [2..n]

choose :: Integer -> Integer -> Integer
choose n k = product [k+1..n] `div` product [2..n-k]

behind1 :: Integer -> Integer
behind1 n = sum [2^(n-2*k)*choose n (2*k)*catalan k | k <- [0..n`div`2]]
