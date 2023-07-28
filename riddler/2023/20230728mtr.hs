possibilities :: [Int] -> [[Int]]
possibilities previousRolls
  | sum previousRolls > 7 = []
  | sum previousRolls == 7 = [previousRolls]
  | otherwise = concatMap (possibilities . (:previousRolls)) [1..6]

main :: IO ()
main = print $ sum $ zipWith (*) probabilities (map fromIntegral nrolls)
  where
    nrolls = map length $ possibilities []
    probabilities :: [Rational]
    probabilities = map (/ (sum p)) p
      where
        p = map ((1/) . fromIntegral . (6^)) nrolls
