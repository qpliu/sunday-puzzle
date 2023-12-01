import Data.Map(Map,fromList,toList,(!))

p :: Map Int Rational
p = table
  where
    table = fromList [(n,pp n) | n <- [-12..39]]
    pp n
      | n == 0 = 1
      | n < 0 = 0
      | otherwise = (table!(n-2) + 2*table!(n-3) + 3*table!(n-4) + 4*table!(n-5) + 5*table!(n-6) + 6*table!(n-7) + 5*table!(n-8) + 4*table!(n-9) + 3*table!(n-10) + 2*table!(n-11) + table!(n-12))/36

main :: IO ()
main = mapM_ print $ map (\ (n,pp) -> (n,pp,fromRational pp)) $ toList p

p3 :: Map Int Rational
p3 = table
  where
    table = fromList [(n,pp n) | n <- [-18..39]]
    pp n
      | n == 0 = 1
      | n < 0 = 0
      | otherwise = (table!(n-3) + 3*table!(n-4) + 6*table!(n-5) + 10*table!(n-6) + 15*table!(n-7) + 21*table!(n-8) + 25*table!(n-9) + 27*table!(n-10) + 27*table!(n-11) + 25*table!(n-12) + 21*table!(n-13) + 15*table!(n-14) + 10*table!(n-15) + 6*table!(n-16) + 3*table!(n-17) + table!(n-18))/216
