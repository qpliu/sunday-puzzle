f :: Integer -> Integer -> Integer
f n m
  | n == 0 || m == 0 = 1
  | n > m = f m n
  | otherwise = sum [f nn (m-1) | nn <- [0..n]]

choose :: Integer -> Integer -> Integer
choose n k = product [k+1..n] `div` product [1..n-k]

r :: Integer -> Integer
r n
  | n <= 1 = 1
  | otherwise = (r (n-1))^2 * choose (2^n-1) (2^(n-1))

rr :: Integer -> Integer
rr n = product [(choose (2^(n-i)-1) (2^(n-i-1)))^(2^i) | i <- [0..n-1]]
