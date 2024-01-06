a1 :: Double -> Double
a1 r = (1-r)*sqrt(1-r^2)

x0 :: Double -> Double
x0 r = sqrt(1-(1-2*r)^2)

y0 :: Double -> Double
y0 r = 1-2*r

y :: Double -> Double
y r = sqrt(1-r^2)*(y0 r + r)/(sqrt(1-r^2) + x0 r) - r

x :: Double -> Double
x r = r*(y r+r)/sqrt((y r)^2 - r^2)

a2 :: Double -> Double
a2 r = 4*x r*(y r+r)

bisect :: Double -> (Double -> Double) -> Double -> Double -> Double
bisect epsilon f x0 x1
  | abs(x0 - x1) < epsilon = x
  | y*y0 < 0 = bisect epsilon f x0 x
  | y*y1 < 0 = bisect epsilon f x x1
  | otherwise = error (show ((x0,y0),(x,y),(x1,y1)))
  where
    x = (x0+x1)/2
    (y,y0,y1) = (f x,f x0,f x1)

main :: IO ()
main = print (r,a1 r,a2 r)
  where r = bisect 0.000001 (\ r -> a1 r - a2 r) 0.1 0.2
