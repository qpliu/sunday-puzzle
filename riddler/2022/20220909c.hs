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

x6 :: Double -> Double
x6 r = 1/2 + (a6 r - b6 r)/2

y6 :: Double -> Double
y6 r = sqrt(r^2 - (a6 r + b6 r)^2/4)

f6 :: Double -> Double
f6 r = y6 r + sqrt(r^2 - (x6 r - r*sqrt 2)^2) - 1 + r*sqrt 2

xyi6 :: Double -> (Double,Double)
xyi6 r = (xa + r*cos(theta+dtheta),ya + r*sin(theta+dtheta))
  where
    xa = x6 r
    ya = 1 - y6 r
    xb = 1 - x6 r
    yb = y6 r
    theta = atan((yb-ya)/(xb-xa))
    dtheta = acos(sqrt((xa-xb)^2+(ya-yb)^2)/(2*r))

f6_2 :: Double -> Double
f6_2 r = r - sqrt((xi-x0)^2 + (yi-y0)^2)
  where
    (xi,yi) = xyi6 r
    x0 = 1 - r/sqrt 2
    y0 = 1 - r/sqrt 2

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
  let r6 = bisect f6 0.0000001 0.30 0.33
  print ("r6",r6)
  print (r6/sqrt 2,r6/sqrt 2)
  print (1-r6/sqrt 2,1-r6/sqrt 2)
  let x6a = (1-2*r6/sqrt 2)/2
  let y6a = sqrt(r6^2-x6a^2)
  print (1-x6a,y6a)
  print (y6a,1-x6a)
  print (x6 r6,1-y6 r6)
  let r6_2 = bisect f6_2 0.0000001 0.301 0.308
  print ("r6-2",r6_2)
  print (r6_2/sqrt 2,r6_2/sqrt 2)
  print (1-r6_2/sqrt 2,1-r6_2/sqrt 2)
  let x6_2a = (1-2*r6_2/sqrt 2)/2
  let y6_2a = sqrt(r6_2^2-x6_2a^2)
  print (1-y6_2a,x6_2a)
  print (y6_2a,1-x6_2a)
  print (x6 r6_2,1-y6 r6_2)
  print (1-x6 r6_2,y6 r6_2)

  let (xi,yi) = xyi6 r6_2
  print (xi,yi)
