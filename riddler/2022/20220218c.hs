w :: Integer -> [Rational] -> [(Integer,Rational,Double)]
w n ps = (n,1-pn,fromRational (1-pn)) : w (n+1) (pn:ps)
  where
    pn = (1 + sum (zipWith f ps [1..]))/(2^n - 1)
    f p k = p*fromIntegral (product [k+1..n])/fromIntegral (product [1..n-k])
