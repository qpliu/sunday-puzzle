expectedX :: Int -> Int -> [Int] -> Rational
expectedX candy minVisitors strategy = fromIntegral (sum [x v | v <- [minVisitors..candy]])/(1+fromIntegral (candy - minVisitors))
  where
    x visitors
      | visitors > length strategy = visitors - length strategy
      | otherwise = candy - sum (take visitors strategy)
