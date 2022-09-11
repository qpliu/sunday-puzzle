a5 :: Double -> Double
a5 r = 1 - 2*sqrt(r^2-1/16)

b5 :: Double -> Double
b5 r = 1 - 4*sqrt(r^2-(a5 r)^2/4)

f5 :: Double -> Double
f5 r = r + sqrt(r^2 - (b5 r)^2/4) - a5 r

bisect :: (Double -> Double) -> Double -> Double -> Double -> Double
bisect f epsilon x1 x2
  | abs (x2 - x1) < epsilon = x3
  | f x1*f x3 < 0 = bisect f epsilon x1 x3
  | f x2*f x3 < 0 = bisect f epsilon x3 x2
  | otherwise = error "error"
  where x3 = (x1 + x2)/2

h6 :: Double -> Double
h6 r = 1 - 2*r/sqrt 2

a6 :: Double -> Double
a6 r = 1/2 - 2*r/sqrt 2

b6 :: Double -> Double
b6 r = 1/2 - 2*sqrt(r^2 - (h6 r)^2/4)

f6 :: Double -> Double
f6 r = sqrt(r^2 - (a6 r - b6 r)^2) + sqrt(r^2 - (a6 r + b6 r)^2/4) - 1/2 - a6 r

bisect6 :: (Double -> Double) -> Double -> Double -> Double -> Double
bisect6 f epsilon x1 x2
  | abs (x2 - x1) < epsilon = x3
  | f x3 /= f x3 || f x3 < 0 = bisect6 f epsilon x3 x2
  | otherwise = bisect6 f epsilon x1 x3
  where x3 = (x1 + x2)/2

main :: IO ()
main = do
  let r5 = bisect f5 0.0000001 0.32 0.4
  print ("r5",r5)
  print (1/4,1-sqrt(r5^2-1/16))
  print (3/4,1-sqrt(r5^2-1/16))
  let x5 = sqrt(r5^2-(a5 r5)^2/4)
  print (x5,sqrt(r5^2-x5^2))
  print (1-x5,sqrt(r5^2-x5^2))
  print (1/2,1-r5-2*sqrt(r5^2-1/16))
  let r6 = 0.305 --bisect6 f6 0.0000001 0.26 0.35
  print ("r6",r6)
  print (r6/sqrt 2,r6/sqrt 2)
  print (1-r6/sqrt 2,1-r6/sqrt 2)
  let x6a = (1-2*r6/sqrt 2)/2
  let y6a = sqrt(r6^2-x6a^2)
  print (1-x6a,y6a)
  print (y6a,1-x6a)
  let x6b = 1/2 + (a6 r6 - b6 r6)/2
  let y6b = sqrt(r6^2 - (a6 r6 + b6 r6)^2/4)
  print (x6b,1-y6b)
