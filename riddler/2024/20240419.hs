import Data.Map(Map,fromList,member,(!))

p :: Int -> Map (Int,Int) Rational
p maxDeckSize = table
  where
    table = fromList [((n,m),prob n m) | n <- [0..maxDeckSize], m <- [-1..n+2]]
    prob n m
      | n <= 0 || m < 0 = 0
      | m >= n = 0
      | otherwise = (nn-mm)/nn^2
                  + (nn-mm)*(nn-mm-1)/nn^2 * table!(n-1,m+1)
                  + 2*(nn-mm)*mm/nn^2 * table!(n-1,m)
                  + mm^2/nn^2 * table!(n-1,m-1)
      where (nn,mm) = (fromIntegral n,fromIntegral m)

main :: IO ()
main = do
    let w = 1 - p 52!(52,0) in print (w,fromRational w)
    let ec = 1 - sum [ecp 52!(52,52-2*i)*ecdeck 52!(52,i) | i <- [0..26]] in print (ec,fromRational ec)

ecdeck :: Int -> Map (Int,Int) Rational
ecdeck deckSize = table
  where
    table = fromList [((n,p),prob n p) | n <- [0..deckSize], p <- [-1..deckSize]]
    prob n p
      | 2*p > n = 0
      | n == 0 && p == 0 = 1
      | n == 0 = 0
      | p < 0 = 0
      | otherwise = (2*d-2*(nn-1)+2*pp)/(2*d-(nn-1)) * table!(n-1,p)
                  + (nn-1-2*(pp-1))/(2*d-(nn-1)) * table!(n-1,p-1)
      where (d,nn,pp) = (fromIntegral deckSize,fromIntegral n,fromIntegral p)

ecp :: Int -> Map (Int,Int) Rational
ecp deckSize = table
  where
    table = fromList [((n,m),prob n m) | n <- [0..deckSize], m <- [-2..n+1]]
    prob n m
      | m <= 0 = 0
      | n <= 0 = 0
      | m > n = 0
      | otherwise = mm/nn^2
                  + mm*(mm-1)/nn^2 * table!(n-1,m-2)
                  + 2*(nn-mm)*mm/nn^2 * table!(n-1,m-1)
                  + (nn-mm)^2/nn^2 * table!(n-1,m)
      where (nn,mm) = (fromIntegral n,fromIntegral m)
