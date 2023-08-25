guaranteedAmount :: Int -> Int -> Rational
guaranteedAmount remainingWins remainingLosses
  | remainingWins < remainingLosses = guaranteedAmount remainingLosses remainingWins
  | remainingLosses <= 0 = 2^remainingWins
  | otherwise = (1+bet)*winAmount
  where
    winAmount = guaranteedAmount (remainingWins-1) remainingLosses
    loseAmount = guaranteedAmount remainingWins (remainingLosses-1)
    bet = (loseAmount-winAmount)/(loseAmount+winAmount)
