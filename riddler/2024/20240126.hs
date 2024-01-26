p2x2 :: Double -> Double -> (Double,(Double,Double))
p2x2 s theta1 = (1-x3,(x1,t))
  where
    theta2 = asin (s*sin theta1)
    theta3 = pi/2 - asin (sin(pi/2-theta2)/s)
    x1 = 1 - tan theta1
    x2 = x1*tan theta2
    x3 = (1-x2)*tan theta3
    t = s*sqrt(1 + (1-x1)^2) + sqrt(x1^2+x2^2) + s*sqrt(x3^2 + (1-x2)^2)

t2x2 :: Double -> (Double,(Double,(Double,Double)))
t2x2 s = (theta1,p2x2 s theta1)
  where
    theta1 = bisect 0.000001 (fst . p2x2 s) (pi/4) (pi/6)

bisect :: Double -> (Double -> Double) -> Double -> Double -> Double
bisect epsilon f x1 x2
  | abs y3 < epsilon = x3
  | signum y1 /= signum y3 = bisect epsilon f x1 x3
  | signum y2 /= signum y3 = bisect epsilon f x2 x3
  | otherwise = error (show ((x1,y1),(x2,y2),(x3,y3)))
  where
    y1 = f x1
    y2 = f x2
    y3 = f x3
    x3 = (x1+x2)/2

p1x2 :: Double -> Double -> (Double,Double)
p1x2 s theta1 = (1-x2,t)
  where
    theta2 = asin (s*sin theta1)
    y1 = tan theta1
    x2 = (1-y1)/tan theta2

    t = 2*s*sqrt(1+y1^2) + 2*sqrt((1-y1)^2 + 1) + 5*sqrt 2

t1x2 :: Double -> (Double,(Double,Double))
t1x2 s = (theta1,p1x2 s theta1)
  where
    theta1 = bisect 0.000001 (fst . p1x2 s) 0.4 0.5

p2x3 :: Double -> Double -> (Double,Double)
p2x3 s theta1 = (1-x4,t)
  where
    theta2 = asin (s*sin theta1)
    theta3 = pi/2 - asin (sin(pi/2-theta2)/s)
    theta4 = asin (s*sin theta3)
    y1 = tan theta1
    x2 = (1-y1)/tan theta2
    y3 = (1-x2)*tan theta3
    x4 = (1-y3)/tan theta4

    t = 2*s*sqrt(1+y1^2) + 2*sqrt((1-y1)^2 + x2^2)
      + 2*s*sqrt((1-x2)^2 + y3^2) + 2*sqrt((1-y3)^2 + 1) + 3*sqrt 2

t2x3 :: Double -> (Double,(Double,Double))
t2x3 s = (theta1,p2x3 s theta1)
  where
    theta1 = bisect 0.000001 (fst . p2x3 s) 0.4 0.5

-- p3x4 :: Double -> Double -> (Double,Double)
p3x4 s theta1 = (1-x6,(t,y1,x2,y3,x4,y5,x6))
  where
    theta2 = asin (s*sin theta1)
    theta3 = pi/2 - asin (sin(pi/2-theta2)/s)
    theta4 = asin (s*sin theta3)
    theta5 = pi/2 - asin (sin(pi/2-theta4)/s)
    theta6 = asin (s*sin theta5)
    y1 = tan theta1
    x2 = (1-y1)/tan theta2
    y3 = (1-x2)*tan theta3
    x4 = (1-y3)/tan theta4
    y5 = (1-x4)*tan theta5
    x6 = (1-y5)/tan theta6

    t = 2*s*sqrt(1+y1^2) + 2*sqrt((1-y1)^2 + x2^2)
      + 2*s*sqrt((1-x2)^2 + y3^2) + 2*sqrt((1-y3)^2 + x4^2)
      + 2*s*sqrt((1-x4)^2 + y5^2) + 2*sqrt((1-y5)^2 + 1) + sqrt 2

-- t2x3 :: Double -> (Double,(Double,Double))
t3x4 s = (theta1,p3x4 s theta1)
  where
    theta1 = bisect 0.000001 (fst . p3x4 s) 0.4 0.5

-- p8x8 :: Double -> Double -> (Double,Double)
p8x8 s theta1 = (1-x6,(t,y1,y3,y5))
 where
    theta2 = asin (s*sin theta1)
    theta3 = pi/2 - asin (sin(pi/2-theta2)/s)
    theta4 = asin (s*sin theta3)
    theta5 = pi/2 - asin (sin(pi/2-theta4)/s)
    theta6 = asin (s*sin theta5)
    y1 = tan theta1
    x2 = (1-y1)/tan theta2
    y3 = (1-x2)*tan theta3
    x4 = (1-y3)/tan theta4
    y5 = (1-x4)*tan theta5
    x6 = (1-y5)/tan theta6

    t = 2*s*sqrt(1+y1^2)
      + 2*sqrt((1-y1)^2 + x2^2) + 2*s*sqrt((1-x2)^2+y3^2)
      + 2*sqrt((1-y3)^2 + x4^2) + 2*s*sqrt((1-x4)^2+y5^2)
      + 2*sqrt((1-y5)^2 + 1) + sqrt(2)

--t8x8 :: Double -> (Double,(Double,Double))
t8x8 s = (theta1,p8x8 s theta1)
  where
    theta1 = bisect 0.000001 (fst . p8x8 s) (pi/4) 0

main :: IO ()
main = do
  print (t2x2 (1/0.9))
  print ()
  print (t1x2 (1/0.9))
  print (t2x3 (1/0.9))
  print (t3x4 (1/0.9))
