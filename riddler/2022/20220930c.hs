epsilon :: Double
epsilon = 0.000000001

bisect :: (Double -> Double) -> Double -> Double -> Double -> Double
bisect f epsilon x1 x2
  | abs (x2 - x1) < epsilon = x3
  | f x1*f x3 < 0 = bisect f epsilon x1 x3
  | f x2*f x3 < 0 = bisect f epsilon x3 x2
  | otherwise = error "error"
  where x3 = (x1 + x2)/2

r :: Double -> Double
r frac = bisect f epsilon 0 1
  where
    f r = (acos r - r*sqrt(1-r^2))/pi - frac

theta :: Double -> Double -> Double
theta r frac = bisect f epsilon (asin r + epsilon) (pi/2 - epsilon)
  where
    f theta = (pi - theta + asin r)/2 + sqrt((1+r/sin theta+sqrt(1-r^2)-r/tan theta)*(-1+r/sin theta+sqrt(1-r^2)-r/tan theta)*(1-r/sin theta+sqrt(1-r^2)-r/tan theta)*(1+r/sin theta-sqrt(1-r^2)+r/tan theta)/16) - pi*frac

pizza :: Double -> (Double,Double,Double,Double)
pizza frac = (rr,thetar,thetar*180/pi,d)
  where
    rr = r frac
    thetar = theta rr 0.4
    d = rr/sin thetar

main :: IO ()
main = do
    print (pizza 0.4)
    print (pizza 0.3)
