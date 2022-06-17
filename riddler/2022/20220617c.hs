import Data.Map(Map,fromList,(!))

c :: Integer -> Integer -> Integer -> Integer -> Rational
c r w rr ww = m!(r,w,rr,ww)
  where
    m :: Map (Integer,Integer,Integer,Integer) Rational
    m = fromList [((r1,w1,rr1,ww1),cc r1 w1 rr1 ww1) | rr1 <- [0..rr], ww1 <- [0..ww], r1 <- [0..r], w1 <- [0..w]]
    cc :: Integer -> Integer -> Integer -> Integer -> Rational
    cc r1 w1 rr1 ww1
      | r1 > rr1 = 0
      | w1 > ww1 = 0
      | r1 == 0 && w1 == 0 = 1
      | r1 == 0 = fromIntegral ww1/fromIntegral (rr1+ww1) * m!(r1,w1-1,rr1,ww1-1)
      | w1 == 0 = fromIntegral rr1/fromIntegral (rr1+ww1) * m!(r1-1,w1,rr1-1,ww1)
      | otherwise = fromIntegral rr1/fromIntegral (rr1+ww1) * m!(r1-1,w1,rr1-1,ww1) + fromIntegral ww1/fromIntegral (rr1+ww1) * m!(r1,w1-1,rr1,ww1-1)

cinf :: Integer -> Integer -> Rational
cinf r w
  | r == 0 && w == 0 = 1
  | r == 0 = (cinf r (w-1))/2
  | w == 0 = (cinf (r-1) w)/2
  | otherwise = (cinf (r-1) w)/2 + (cinf r (w-1))/2

main :: IO ()
main = do
  mapM_ print [let ch = c 8 11 n n in (2*n,ch,fromRational ch :: Double) | n <- [1..100]]
  print (let ch = cinf 8 11 in (ch,fromRational ch))
