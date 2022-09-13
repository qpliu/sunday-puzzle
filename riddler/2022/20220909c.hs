bisect :: (Double -> Double) -> Double -> Double -> Double -> Double
bisect f epsilon x1 x2
  | abs (x2 - x1) < epsilon = x3
  | f x1*f x3 < 0 = bisect f epsilon x1 x3
  | f x2*f x3 < 0 = bisect f epsilon x3 x2
  | otherwise = error "error"
  where x3 = (x1 + x2)/2

f5 :: Double -> (Double,[(Double,Double)])
f5 r = (f,[(x1,y1),(1-x1,y1),(x2,y2),(1-x2,y2),(x3,y3)])
  where
    x1 = 1/4
    y1 = 1 - sqrt(r^2 - x1^2)
    y2 = (1-2*(1-y1))/2
    x2 = sqrt(r^2 - y2^2)
    w = 1 - 4*x2
    x3 = 1/2
    y3 = sqrt(r^2 - w^2/4)
    f = y3+r - (1 - 2*sqrt(r^2 - x1^2))

f6a :: Double -> (Double,[(Double,Double)])
f6a r = (f,[(x1,y1),(1-x1,1-y1),(x2,y2),(y2,x2),(x3,y3),(y3,x3)])
  where
    x1 = r/sqrt 2
    y1 = r/sqrt 2
    y2 = 1-(1-2*y1)/2
    x2 = sqrt(r^2 - (1-y2)^2)
    x3 = (2*x2 + (1 - 2*x1))/2
    w = 1 - 2*x1 - 2*x2
    y3 = 1 - sqrt(r^2 - w^2/4)
    f = y3 - sqrt(r^2 - (x3 - 2*x1)^2) - 2*y1

f6b :: Double -> (Double,[(Double,Double)])
f6b r = (f,[(x1,y1),(1-x1,1-y1),(x2,y2),(1-x2,1-y2),(x3,y3),(1-x3,1-y3)])
  where
    x1 = r/sqrt 2
    y1 = r/sqrt 2
    y2 = 1-(1-2*y1)/2
    x2 = sqrt(r^2 - (1-y2)^2)
    x3 = (2*x2 + (1 - 2*x1))/2
    w = 1 - 2*x1 - 2*x2
    y3 = 1 - sqrt(r^2 - w^2/4)
    theta = atan((1-y3-y3)/(1-x3-x3))
    dtheta = acos(sqrt((1-x3-x3)^2+(1-y3-y3)^2)/(2*r))
    xi = x3 + r*cos(theta+dtheta)
    yi = y3 + r*sin(theta+dtheta)
    f = r - sqrt((1-x1-xi)^2+(1-y1-yi)^2)

f6c :: Double -> Double -> (Double,[(Double,Double)])
f6c phi r = (f,[(x1,y1),(1-x1,1-y1),(x2,y2),(1-x2,1-y2),(x3,y3),(1-x3,1-y3)])
  where
    x1 = r*cos phi
    y1 = r*sin phi
    y2 = 1-(1-2*y1)/2
    x2 = sqrt(r^2 - (1-y2)^2)
    x3 = (2*x2 + (1 - 2*x1))/2
    w = 1 - 2*x1 - 2*x2
    y3 = 1 - sqrt(r^2 - w^2/4)
    theta = atan((1-y3-y3)/(1-x3-x3))
    dtheta = acos(sqrt((1-x3-x3)^2+(1-y3-y3)^2)/(2*r))
    xi = x3 + r*cos(theta+dtheta)
    yi = y3 + r*sin(theta+dtheta)
    f = r - sqrt((1-x1-xi)^2+(1-y1-yi)^2)

f6d :: Double -> Double -> (Double,[(Double,Double)])
f6d phi r = (f,[(x1,y1),(1-x1,y1),(x2,y2),(1-x2,y2),(x3,y3),(x4,y4)])
  where
    x1 = r*cos phi
    y1 = r*sin phi
    y2 = 1-(1-2*y1)/2
    x2 = sqrt(r^2 - (1-y2)^2)
    x3 = 1/2
    w3 = 1 - 4*x1
    y3 = sqrt(r^2 - w3^2/4)
    x4 = 1/2
    w4 = 1 - 4*x2
    y4 = 1 - sqrt(r^2 - w4^2/4)
    yi = (y3+y4)/2
    xi = 1/2 - sqrt(r^2 - (y3-y4)^2/4)
    f = r - sqrt((xi-x2)^2 + (yi-y2)^2)

gen :: (Double -> (Double,[(Double,Double)])) -> Double -> Double -> Double -> String
gen f epsilon r1 r2 = svg rmin ((snd . f) rmin)
  where
    rmin = bisect (fst . f) epsilon r1 r2

svg :: Double -> [(Double,Double)] -> String
svg r circles = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" width=\"400\" height=\"400\" viewBox=\"-50 -50 200 200\">\n<rect x=\"0\" y=\"0\" width=\"100\" height=\"100\" fill=\"transparent\" stroke=\"black\"/>\n" ++ concatMap circle circles ++ "</svg>\n"
  where
    circle (x,y) = "<circle cx=\"" ++ show (100*x) ++ "\" cy=\"" ++ show (100*y) ++ "\" r=\"" ++ show (100*r) ++ "\" fill=\"transparent\" stroke=\"black\"/>"

main :: IO ()
main = do
  putStr (gen f5 0.0000001 0.30 0.35)
  putStr (gen f6a 0.0000001 0.30 0.35)
  putStr (gen f6b 0.0000001 0.301 0.31)
  let (r6c,phi6c) = minimum [(bisect (fst . f6c (pi/4+phi)) 0.0000001 0.288 0.3,phi+pi/4) | phi <- [0.125,0.12505 .. 0.135]]
  print (r6c,phi6c,phi6c*180/pi)
  putStr (gen (f6c phi6c) 0.0000001 0.288 0.3)
  let (r6d,phi6d) = minimum [(bisect (fst . f6d (pi/4+phi)) 0.0000001 0.288 0.305,phi+pi/4) | phi <- [0.140,0.14005 .. 0.145]]
  print (r6d,phi6d,phi6d*180/pi)
  putStr (gen (f6d phi6d) 0.0000001 0.288 0.305)
