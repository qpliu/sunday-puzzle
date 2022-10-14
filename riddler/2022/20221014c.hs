nexpected :: Integer -> Rational
nexpected n
  | n >= 365 = 366
  | otherwise = fromIntegral (n*(n+1))/365 + (365 - fromIntegral n)*nexpected (n+1)/365

main :: IO ()
main = print (nexpected 0)
