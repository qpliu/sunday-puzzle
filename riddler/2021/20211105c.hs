cm :: Integer -> Integer
cm inches = (254*inches + 50) `div` 100

sumFactors :: Integer -> Integer
sumFactors n = s 0 1
  where
    s total m | q < m = total
              | n `mod` m /= 0 = s total (m+1)
              | q /= m = s (total + q + m) (m+1)
              | otherwise = total + q
      where q = n `div` m

main :: IO ()
main = mapM_ print [i | i <- [1..50000], cm i == sumFactors i]
