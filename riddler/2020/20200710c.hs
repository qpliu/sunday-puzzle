s :: Integer -> Integer -> Integer
s 0 _ = 1
s n m = 1 + m*s (n-1) m + sum [s (i-1) (m+n-i) | i <- [1..n]]

main :: IO ()
main = mapM_ print [(n,s n 0 - 1) | n <- [1..20]]
