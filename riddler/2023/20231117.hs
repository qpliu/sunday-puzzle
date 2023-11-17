import Data.List(nub,sort)

p :: (Int,Int,Int,Int) -> Rational
p (k,l,m,n) = product [1..8]*3^(l+m)/(8^8*product [1..fromIntegral k]*product [1..fromIntegral l]*product [1..fromIntegral m]*product [1..fromIntegral n])

pp :: (Int,Int,Int,Int) -> Rational
pp (a,b,c,d) = sum $ map p $ nub [
    (a,b,c,d),(a,b,d,c),(a,c,b,d),(a,c,d,b),(a,d,b,c),(a,d,c,b),
    (b,a,c,d),(b,a,d,c),(b,c,a,d),(b,c,d,a),(b,d,a,c),(b,d,c,a),
    (c,a,b,d),(c,a,d,b),(c,b,a,d),(c,b,d,a),(c,d,a,b),(c,d,b,a),
    (d,a,b,c),(d,a,c,b),(d,b,a,c),(d,b,c,a),(d,c,a,b),(d,c,b,a)]

main :: IO ()
main = mapM_ print $ sort [((a,b,c,d),pp (a,b,c,d),fromRational (pp (a,b,c,d))) | a <- [0..8], b <- [0..8], b <= a, c <- [0..8], c <= b, d <- [0..8], d <= c, a + b + c + d == 8]
