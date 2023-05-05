expectedD :: Integer -> Integer -> Rational -> Rational
expectedD limit n d
  | n > limit = d
  | 2*jump < d = d
  | otherwise = expectedD limit (n+1) (d+jump)/2 + expectedD limit (n+1) (abs (d-jump))/2
  where jump = 2^n/3^n

main :: IO ()
main = mapM_ print [(limit,fromRational (expectedD limit 2 (2/3)) :: Double) | limit <- [2..30]]
