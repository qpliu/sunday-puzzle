import Data.Map(Map,fromList,(!))

catalan :: Integer -> Integer
catalan n = product [n+2..2*n] `div` product [1..n]

tTable :: Integer -> Map Integer Integer
tTable n = table
  where
    table = fromList [(i,makeT i) | i <- [0..n]]
    makeT n
      | n == 0 = 1
      | n == 1 = 1
      | otherwise = sum [(catalan i)^2*table!(n-1-i) | i <- [0..n-1]]

s :: Integer -> Integer
s n = sum [(t!i)^2*(t!(n-1-i))^2 | i <- [0..n-1]]
  where
    t = tTable n
