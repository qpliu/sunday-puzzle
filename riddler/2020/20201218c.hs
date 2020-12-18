search :: (Rational,Rational) -> (Rational,Rational)
search (lbound,ubound) = run 1 mid
  where
    mid = (lbound + ubound) / 2
    run a b | b / a <= lbound = (mid,ubound)
            | b / a >= ubound = (lbound,mid)
            | otherwise = run (a - (b - a)) (b - a)

main :: IO ()
main = print (fromRational l :: Double,fromRational u :: Double,l,u)
  where (l,u) = head (drop 100 (iterate search (1,2)))
