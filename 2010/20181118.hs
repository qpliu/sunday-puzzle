digits :: Integer -> Integer -> [Integer]
digits n base | n == 0 = [] | otherwise = n `mod` base : digits (n `div` base) base

series :: [Integer]
series = 1 : map (\ n -> n + sum (digits n 10)) series

main :: IO ()
main = print (series !! 7)

-- 38
