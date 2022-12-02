import Data.Map(fromList,(!))

bus :: (Int,Int) -> [((Int,Int),Rational)]
bus (m,n)
  | m == 0 || n == 0 = [((0,0),1)]
  | otherwise = [((m-p,n),chance (fromIntegral p) mm nn) | p <- [1..m]]
             ++ [((m,n-p),chance (fromIntegral p) nn mm) | p <- [1..n]]
  where
    (mm,nn) = (fromIntegral m,fromIntegral n)
    chance p m n = m/(m+n)*n/(m+n-1)*product [(m-q+1)/(m+n-q) | q <- [2..p]]

lastIsM :: (Int,Int) -> Rational
lastIsM (m,n) = memo!(m,n)
  where
    memo = fromList [((mm,nn),chance mm nn) | mm <- [0..m], nn <- [0..n]]
    chance m n
      | m == 0 = 0
      | n == 0 = 1
      | otherwise = sum [p*memo!fans | (fans,p) <- bus (m,n)]

buses :: (Int,Int) -> Rational
buses (m,n) = memo!(m,n)
  where
    memo = fromList [((mm,nn),count mm nn) | mm <- [0..m], nn <- [0..n]]
    count m n
      | m == 0 || n == 0 = 1
      | otherwise = 1 + sum [p*memo!fans | (fans,p) <- bus (m,n)]

main :: IO ()
main = do
  print $ lastIsM (11,7)
  print $ buses (11,7)
