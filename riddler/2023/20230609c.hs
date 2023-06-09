b :: Integer -> Double -> (Double,Double)
b n a = ((lognfac-log 100)/d,(lognfac+log 100)/d)
  where
    lognfac = sum [log (fromIntegral k) | k <- [1..n]]
    d = fromIntegral n*log(a*fromIntegral n)

brange :: Double -> Maybe (Double,Double)
brange a
  | bmin < bmax = Just (bmin,bmax)
  | otherwise = Nothing
  where
    bs = [b n a | n <- [4..200]]
    bmin = maximum (map fst bs)
    bmax = minimum (map snd bs)
