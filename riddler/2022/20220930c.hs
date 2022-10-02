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

pizza :: Double -> Double -> Double -> (Double,Double,Double,Double,Double)
pizza ra rb frac = (ra,rb,theta,theta*180/pi,dist)
  where
    theta = asin(ra/dist) + asin(rb/dist)
    dist = bisect f epsilon (max ra rb + epsilon) (1-epsilon)
    f d = 2*pi - acos ra - acos rb - acos(ra/d) - acos(rb/d) + ra*sqrt(1-ra^2) + rb*sqrt(1-rb^2) + ra*sqrt(d^2-ra^2) + rb*sqrt(d^2-rb^2) - 2*pi*frac

svg :: (Double,Double,Double,Double,Double) -> String
svg (ra,rb,theta,_,d) =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
    "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" width=\"440\" height=\"440\" viewBox=\"-220 -220 440 440\">\n" ++
    "  <circle cx=\"0\" cy=\"0\" r=\"200\" fill=\"transparent\" stroke=\"black\"/>\n" ++
    "  <line x1=\"" ++ show (ra*200) ++ "\" y1=\"-200\" x2=\"" ++ show (ra*200) ++ "\" y2=\"200\" stroke=\"black\"/>\n" ++
    "  <line x1=\"" ++ show (x1*200) ++ "\" y1=\"" ++ show (y1*200) ++ "\" x2=\"" ++ show (x2*200) ++ "\" y2=\"" ++ show (y2*200) ++ "\" stroke=\"black\"/>\n" ++
    "</svg>"
  where
    xi = ra
    yi = -sqrt(d^2-ra^2)
    -- y = yi + (x - xi)/tan theta
    x1 = -1
    x2 = 1
    y1 = yi + (x1 - xi)/tan theta
    y2 = yi + (x2 - xi)/tan theta

main :: IO ()
main = do
    print $ pizza 0 (r 0.4) 0.2
    putStrLn $ svg $ pizza 0 (r 0.4) 0.2
    print $ pizza 0 (r 0.3) 0.3
    putStrLn $ svg $ pizza 0 (r 0.3) 0.3
    print $ pizza (r 0.3) (r 0.4) 0.4
    putStrLn $ svg $ pizza (r 0.3) (r 0.4) 0.4
