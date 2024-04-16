
bisect :: Double -> (Double -> Double) -> Double -> Double -> Double
bisect epsilon f x0 x1
  | abs(x0 - x1) < epsilon = x
  | y*y0 < 0 = bisect epsilon f x0 x
  | y*y1 < 0 = bisect epsilon f x x1
  | otherwise = error (show ((x0,y0),(x,y),(x1,y1)))
  where
    x = (x0+x1)/2
    (y,y0,y1) = (f x,f x0,f x1)

a :: Double -> Double
a alpha = 2*sin(alpha/2)/sqrt 3

b :: Double -> Double
b alpha = 2*cos(alpha/2)/3

d :: Double -> Double -> Double
d alpha beta = 2*aa^2*bb*cc/(bb^2*ss^2 + aa^2*cc^2)
  where
    gamma = (alpha + beta)/2
    aa = a alpha
    bb = b alpha
    cc = cos gamma
    ss = sin gamma

alpha5a :: Double -> Double
alpha5a epsilon = bisect epsilon f (2*pi/7) (2*pi/5)
  where
    f alpha = cc^2/aa^2 + (ss - bb - dd)^2/bb^2 - 1
      where
        beta = pi - 3*alpha/2
        aa = a alpha
        bb = b alpha
        dd = d alpha beta
        cc = cos(beta/2)
        ss = sin(beta/2)

alpha5b :: Double -> Double
alpha5b epsilon = bisect epsilon f (2*pi/7) (2*pi/5)
  where
    f alpha = cc^2/bb^2 + (ss - aa - dd)^2/aa^2 - 1
      where
        beta = pi - 3*alpha/2
        aa = a alpha
        bb = b alpha
        dd = d alpha beta
        cc = cos(beta/2)
        ss = sin(beta/2)

alpha6a :: Double -> Double
alpha6a epsilon = bisect epsilon f (2*pi/8) (2*pi/6)
  where
    f alpha = cc^2/aa^2 + (ss - bb - dd)^2/bb^2 - 1
      where
        beta = pi/3 - alpha
        aa = a alpha
        bb = b alpha
        dd = d alpha beta
        cc = cos(beta/2)
        ss = sin(beta/2)

alpha6b :: Double -> Double
alpha6b epsilon = bisect epsilon f (2*pi/8) (2*pi/6)
  where
    f alpha = cc^2/bb^2 + (ss - aa - dd)^2/aa^2 - 1
      where
        beta = pi/3 - alpha
        aa = a alpha
        bb = b alpha
        dd = d alpha beta
        cc = cos(beta/2)
        ss = sin(beta/2)

v5 alpha = (alpha*180/pi,beta*180/pi,(alpha+beta)*90/pi,(3*alpha+beta)*90/pi,a alpha,b alpha,d alpha beta + b alpha,cos(beta/2))
  where
    beta = pi - 3*alpha/2
