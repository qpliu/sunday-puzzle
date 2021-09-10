c :: Double -> Double -> Rational
c m n | m <= 1 || n <= 1 = 0
      | otherwise = 1 + c (m-1) n / 2 + c m (n-1) / 2

main :: IO ()
main = print (c 8.5 11)
