bigP :: Rational -> Rational -> Rational
bigP p i
  | i <= 2 = 1
  | otherwise = 1 + (1-p)*bigP p (i-1)

v99 :: Rational -> Rational
v99 p = (bigP p 99 + (1-p)^98)/(1 - p * bigP p 99)

vn :: Rational -> Double
vn n = fromRational ((bigP (1/n) n + (1-1/n)^(floor n-1))/(1 - bigP (1/n) n/n)/n)
