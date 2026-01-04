find :: String -> Rational -> [Rational] -> [String]
find suffix target digits = concatMap try (splits 1 0 digits)
  where
    try (n,[])
      | n == target = [show (floor n) ++ suffix]
      | otherwise = []
    try (n,digits) =
        find ("+" ++ show (floor n) ++ suffix) (target - n) digits
            ++ find ("-" ++ show (floor n) ++ suffix) (target + n) digits
            ++ find ("×" ++ show (floor n) ++ suffix) (target / n) digits
            ++ find ("÷" ++ show (floor n) ++ suffix) (target * n) digits

splits :: Rational -> Rational -> [Rational] -> [(Rational,[Rational])]
splits place num [] = []
splits place num (digit:digits) =
    (nextNum,digits) : splits (10*place) nextNum digits
  where nextNum = place*digit+num

main = do
     mapM_ putStrLn (find " = 2011" 2011 [9,8..1])
     mapM_ putStrLn (find " = 2026" 2026 [9,8..1])

-- (12-3)×4×56-7+8+9 = 2026
-- (1+2)×3×4×56-7+8+9 = 2026
-- (1×2+34)×56-7+8+9 = 2026
-- 12×34×5-6-7+8-9 = 2026
