import Data.Map(fromList,(!))

nexpected :: Integer -> Rational
nexpected n
  | n >= 365 = 366
  | otherwise = fromIntegral (n*(n+1))/365 + (365 - fromIntegral n)*nexpected (n+1)/365

nextra :: Rational -> Rational
nextra year = memo!(0,0)
  where
    memo = fromList [((n,m),nexp n m) | n <- [0..2*year], m <- [0..min n year]]
    nexp n m
      | m >= year = 2*year+1
      | otherwise = ((n-m)*(n+1))/year + (if 2*m > n then (2*m-n)*memo!(n+1,m)/year else 0) + (year - m)*memo!(n+1,m+1)/year

main :: IO ()
main = do
  let x = nexpected 0 in print (x, fromRational x :: Double)
  let x = nextra 365 in print (x, fromRational x :: Double)
