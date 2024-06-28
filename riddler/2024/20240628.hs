import Data.Map(Map,fromList,toList,(!))
import Data.Tuple(swap)

pTable :: Integer -> Map Integer Rational
pTable np = table
  where
    table = fromList [(n,p n) | n <- [1 .. np+1]]
    p n
      | n < 2 = 0
      | otherwise = sum [(1-table!i)*fromIntegral (i-1)/fromIntegral (2*np-i+1) | i <- [1..n-1]]

ppTable :: Integer -> Map Integer Rational
ppTable np = table
  where
    p = pTable np
    table = fromList [(n,pp n) | n <- [1..np+1]]
    pp n = (1 - p!n)*fromIntegral (n-1)/fromIntegral (2*np-n+1)

mp :: Integer -> (Rational,Integer)
mp nn = maximum $ map swap $ toList $ ppTable nn

p :: Integer -> Integer -> Rational
p np n = 2^n*fromIntegral (product [np-n+1..np])/fromIntegral (product [2*np-n+1..2*np])

pp :: Integer -> Integer -> Rational
pp np n = 2^n*fromIntegral (n*product [np-n+1..np])/fromIntegral (product [2*np-n .. 2*np])

main :: IO ()
main = do
    mapM_ (print . f) [10,100,1000,2000,3000]
  where
    f np = (sqrt (2*fromIntegral np),maximum [(pp np n,n) | n <- [1..np]])
