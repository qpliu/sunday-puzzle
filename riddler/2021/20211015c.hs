series :: Int -> [(Rational,[Bool])]
series n = map (\ (chance,games) -> (chance,reverse games)) (s (1,[]))
  where
    s :: (Rational,[Bool]) -> [(Rational,[Bool])]
    s (chance,games)
      | length ((filter id) games) >= nwin || length ((filter not) games) >= nwin = [(chance,games)]
      | otherwise = s (chance/2,True:games) ++ s (chance/2,False:games)
    nwin = n `div` 2 + 1

win :: Int -> Int -> [Bool] -> Rational
win nseries npredict prediction = sum [chance | (chance,games) <- series nseries, length (filter id (zipWith (==) prediction games)) >= npredict]

predictions :: Int -> [Bool] -> [[Bool]]
predictions n previous
  | n <= 0 = [previous]
  | otherwise = predictions (n-1) (True:previous) ++ predictions (n-1) (False:previous)

bestWinChance :: Int -> Int -> Rational
bestWinChance nseries npredict = maximum [win nseries npredict prediction | prediction <- predictions nseries []]

bestWins :: Int -> Int -> [[Bool]]
bestWins nseries npredict = [prediction | prediction <- predictions nseries [], win nseries npredict prediction == bestWinChance nseries npredict]

main :: IO ()
main = print (bestWinChance 7 2)
