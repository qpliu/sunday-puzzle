import Data.List(nub)
import Data.Map(Map,findWithDefault,fromList,keys)

p :: Int -> Map (Int,Int,Int) Rational
p tmax = table
  where
    table = fromList [((n,i,t),pp n i t) | t <- [0..tmax], n <- [0..t], i <- [0..n `div` 2]]
    pp 0 0 0 = 1
    pp 0 0 t = f 1 0 t
    pp n 0 t = (f (n-1) 0 t
                + 2*f n 1 t
                + f (n+1) 0 t
                + 2*f (n+1) 1 t)/6
    pp n i t = (f (n-1) (i-1) t
                + f (n-1) i t
                + f n (i-1) t
                + f n (i+1) t
                + f (n+1) i t
                + f (n+1) (i+1) t)/6
    f n i t = findWithDefault 0 (n,ci n i,t-1) table

ci :: Int -> Int -> Int
ci 0 _ = 0
ci n i = min (i `mod` n) (n - i `mod` n)

main :: IO ()
main = do
    let table = p 100
    let f t = findWithDefault 0 (0,0,t) table
    mapM_ print [(t,f t,f t > 1/100) | t <- [0..100]]
