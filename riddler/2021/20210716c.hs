n6 :: Rational -> Rational
n6 p = 1/(p-p*p)

avgShots :: Rational -> Int -> Int -> Int -> Rational
avgShots p round score1 score2
  | round > 5 = if score1 == score2 then n6 p else 0
  | 6-round < abs (score1-score2) = 0
  | 6-round == abs (score1-score2) =
    -- first shot in the round could end it
    -- end on score if the leading team goes first
      1/2*p*1 +
    -- end on miss if the trailing team goes first
      1/2*(1-p)*1 +
    -- otherwise both shots are taken in the round
      1/2*p*p*(2 + avgShots p (round+1) (score1+1) (score2+1)) +
      1/2*(1-p)*(1-p)*(2 + avgShots p (round+1) score1 score2) +
      1/2*p*(1-p)*(2 + avgShots p (round+1) (score1+1) score2) +
      1/2*(1-p)*p*(2 + avgShots p (round+1) score1 (score2+1))
  | otherwise =
      p*p*(2 + avgShots p (round+1) (score1+1) (score2+1)) +
      (1-p)*(1-p)*(2 + avgShots p (round+1) score1 score2) +
      p*(1-p)*(2 + avgShots p (round+1) (score1+1) score2) +
      (1-p)*p*(2 + avgShots p (round+1) score1 (score2+1))

main :: IO ()
main = do
    print $ let a = avgShots (7/10) 1 0 0 in (fromRational a :: Double, a)
    mapM_ print [let a = avgShots (fromIntegral n/10) 1 0 0 in (fromRational a :: Double,a,n) | n <- [1..9]]
