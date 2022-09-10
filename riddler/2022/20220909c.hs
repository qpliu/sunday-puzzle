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
h6 r = 1 - 2*r*sqrt 2

a6 :: Double -> Double
a6 r = 1/2 - 2*r*sqrt 2

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
  print ("r5",bisect f5 0.0000001 0.32 0.4)
  print ("r6",bisect6 f6 0.0000001 0.25 0.35)
