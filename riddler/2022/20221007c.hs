import Data.Map(fromList,(!))

success :: Integer -> Integer -> Rational
success pairs spots = 1 - f pairs 0
  where
    memo = fromList [((n,m),f n m) | n <- [0..pairs], m <- [0..spots]]
    f n m | n == 0 = 0
          | m == 0 = memo!(n-1,m+1)
          | m >= spots = (2*fromIntegral n + fromIntegral m*memo!(n,m-1)) / fromIntegral (2*n+m)
          | otherwise = (2*fromIntegral n*memo!(n-1,m+1) + fromIntegral m*memo!(n,m-1)) / fromIntegral (2*n+m)

main :: IO ()
main = print $ success 14 9
