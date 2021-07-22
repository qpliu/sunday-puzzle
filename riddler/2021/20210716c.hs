n6 :: Rational -> Rational
n6 p = 1/(p-p*p)

avgShots :: Rational -> Int -> Int -> Int -> Rational
avgShots p round score1 score2
  | round > 5 = if score1 == score2 then n6 p else 0
  | 6-round < abs (score1-score2) = 0
  | 6-round == score1-score2 =
    -- end on score when the leading team goes first
      p*1 +
      (1-p)*p*(2 + avgShots p (round+1) score1 (score2+1)) +
      (1-p)*(1-p)*(2 + avgShots p (round+1) score1 score2)
  | 6-round == score2-score1 =
    -- end on miss when the trailing team goes first
      (1-p)*1 +
      p*p*(2 + avgShots p (round+1) (score1+1) (score2+1)) +
      p*(1-p)*(2 + avgShots p (round+1) (score1+1) score2)
  | otherwise =
      p*p*(2 + avgShots p (round+1) (score1+1) (score2+1)) +
      (1-p)*(1-p)*(2 + avgShots p (round+1) score1 score2) +
      p*(1-p)*(2 + avgShots p (round+1) (score1+1) score2) +
      (1-p)*p*(2 + avgShots p (round+1) score1 (score2+1))

main :: IO ()
main = do
    print $ let a = avgShots (7/10) 1 0 0 in (fromRational a :: Double, a)
    mapM_ print [let a = avgShots p 1 0 0 in (p,fromRational a :: Double,a,fromRational (a - n6 p) :: Double) | p <- [1/10,2/10..9/10] ++ [95/100,99/100,999/1000,9999/10000,1/10000]]
