import Data.List(intercalate)

al :: Double -> Double
al a = sqrt(a^2 + (beta a)^2)

bl :: Double -> Double
bl b = sqrt((alpha b)^2 + b^2)

alpha :: Double -> Double
alpha b = (-2*b^3 + 6*b^2 - 3*b - sqrt(4*b^6 - 24*b^5 + 57*b^4 - 66*b^3 + 34*b^2))/(3*b - 5)

beta :: Double -> Double
beta a = (-2*a^2 + a + sqrt(a^2*(4*a^2-4*a+17)))/4

svg :: (Double,Double) -> String
svg (a,b) = "<line x1=\"0.5\" y1=\"2\" stroke=\"#822\" stroke-width=\"0.01\">"
    ++ "<animate attributeName=\"x2\" dur=\"5s\" repeatCount=\"indefinite\" "
    ++ "values=\"" ++ intercalate ";" (map (\ (x,_,_,_) -> show x) points)
    ++ "\"/>"
    ++ "<animate attributeName=\"y2\" dur=\"5s\" repeatCount=\"indefinite\" "
    ++ "values=\"" ++ intercalate ";" (map (\ (_,y,_,_) -> show y) points)
    ++ "\"/></line>"
    ++ "<line stroke=\"#822\" stroke-width=\"0.01\">"
    ++ "<animate attributeName=\"x1\" dur=\"5s\" repeatCount=\"indefinite\" "
    ++ "values=\"" ++ intercalate ";" (map (\ (x,_,_,_) -> show x) points)
    ++ "\"/>"
    ++ "<animate attributeName=\"y1\" dur=\"5s\" repeatCount=\"indefinite\" "
    ++ "values=\"" ++ intercalate ";" (map (\ (_,y,_,_) -> show y) points)
    ++ "\"/>"
    ++ "<animate attributeName=\"x2\" dur=\"5s\" repeatCount=\"indefinite\" "
    ++ "values=\"" ++ intercalate ";" (map (\ (_,_,x,_) -> show x) points)
    ++ "\"/>"
    ++ "<animate attributeName=\"y2\" dur=\"5s\" repeatCount=\"indefinite\" "
    ++ "values=\"" ++ intercalate ";" (map (\ (_,_,_,y) -> show y) points)
    ++ "\"/></line>"
  where
    theta = atan2 b a
    points = let xs = [0,0.05..a] in map ray (xs ++ a : reverse xs)
    ray x = (x,y,x2,y2)
      where
        y = b-b*x/a
        phi = atan2 (0.5-x) (2-y)
        m = tan(2*theta - phi)
        y2 | m > 0.00001 = 1
           | m < -0.00001 = 0
           | otherwise = y
        x2 | abs m > 0.00001 = x + (y2-y)*m
           | otherwise = 5

ab :: (Double,Double)
ab = (a,a) where a = (3-sqrt 3)/2

abec :: (Double,Double)
abec = (a,beta a)
  where
    cbrt x = exp(log x/3)
    c = cbrt(838 + 15*sqrt 21669)
    a = (4 - 161/c + c)/12
