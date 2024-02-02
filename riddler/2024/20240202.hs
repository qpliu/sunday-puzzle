f :: Integer -> Int
f n
  | n < 10 = 0
  | otherwise = 1 + f (z n)
  where
    z n
      | n <= 0 = 0
      | otherwise = (n `mod` 10) + z (n `div` 10)

main :: IO ()
main = print $ length [() | n <- [1..10000], f n == 3]
