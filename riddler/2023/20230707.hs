faces :: Int -> Rational
faces n = fromIntegral (sum [gcd n m | m <- [1..n]])/fromIntegral n

main :: IO ()
main = print $ maximum [(faces n,n) | n <- [1..1000]]
