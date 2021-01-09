p :: Integer -> Rational
p i = (2*fromIntegral i - 1)/100

s :: Integer -> Rational
s i = sum [p i'*s i'|i' <- [1..i-1]] + 1

main :: IO ()
main = print (s 11,fromRational (s 11) :: Double)
