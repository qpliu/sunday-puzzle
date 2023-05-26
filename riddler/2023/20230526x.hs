r :: Rational -> Rational
r d = fromIntegral (round (100/d))/100

main :: IO ()
main = print $ maximum [(d*r d,d,r d) | d <- map (/100) [1..99]]
