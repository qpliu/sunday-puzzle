ab :: Int -> Int -> (Rational,Rational)
ab bign n
  | n >= bign-1 = (1,(bignn-1)/bignn)
  | otherwise = ((bignn + (bignn-nn)*aplus)/denominator, nn/denominator)
  where
    (aplus,bplus) = ab bign (n+1)
    (bignn,nn) = (fromIntegral bign, fromIntegral n)
    denominator = bignn - (bignn - nn)*bplus

ee :: Int -> Rational
ee bign
  | bign <= 1 = 1
  | otherwise = (1 + a1)/(1 - b1)
  where
    (a1,b1) = ab bign 1

main :: IO ()
main = mapM_ print [let r = ee (n+1)/ee n in (n,r,fromRational r) | n <- [1..100]]
