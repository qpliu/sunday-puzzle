step :: Double -> (Double,Double,Double) -> (Double,Double,Double)
step dtheta (r,theta,_)
  | r^2 > 4 / (cos theta)^4 = error (show (r,theta))
  | r1^2 <= 4 / (cos theta1)^4 = (r1,theta1,rmax)
  | r2^2 <= 4 / (cos theta1)^4 = (r2,theta1,-1)
  | otherwise = (sqrt (2/(cos theta1)^2),theta1,-2)
  where
    dr = dtheta * sqrt (4 / (cos theta)^4 - r^2)
    r1 = r + dr
    r2 = r - dr
    theta1 = theta + dtheta
    rmax = 2 / (cos theta)^2


full :: (a,Double,a) -> Bool
full (_,theta,_) = theta >= pi/4

half :: (a,Double,a) -> Bool
half (_,theta,_) = theta >= 0

integrate :: Double -> ((Double,Double,Double) -> Bool) -> Double
integrate dtheta done = (\ (r,_,_) -> r) $ last $ takeWhile (not . done) $ iterate (step dtheta) (1,-pi/4,0)

main :: IO ()
main = do
    print "half"
    print (integrate 0.1 half)
    print (integrate 0.01 half)
    print (integrate 0.001 half)
    print (integrate 0.0001 half)
    print (integrate 0.00001 half)
    print (integrate 0.000001 half)
    print "full"
    print (integrate 0.1 full)
    print (integrate 0.01 full)
    print (integrate 0.001 full)
    print (integrate 0.0001 full)
    print (integrate 0.00001 full)
    print (integrate 0.000001 full)
