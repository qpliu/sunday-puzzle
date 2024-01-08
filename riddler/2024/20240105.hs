x :: (Double,Double,Double) -> Double
x (r,h,y) = r*(y+h)/sqrt(y^2-r^2)

outerarea :: (Double,Double,Double) -> Double
outerarea (r,h,y) = (1-r)*sqrt(1-r^2)

y0 :: (Double,Double,Double) -> Double
y0 rhy@(r,h,y) = (-b+sqrt(b^2-4*a*c))/(2*a)
  where
    a = (y+h)^2 + 1-h^2
    b = -2*y*(1-h^2)
    c = y^2*(1-h^2) - (y+h)^2

x0 :: (Double,Double,Double) -> Double
x0 rhy = sqrt(1-(y0 rhy)^2)

redarea :: (Double,Double,Double) -> Double
redarea rhy@(r,h,y) = (y0 rhy+h)*sqrt(1-h^2)

xblue :: (Double,Double,Double) -> Double
xblue (r,h,y) = sqrt(1-h^2)

xblack :: (Double,Double,Double) -> Double
xblack rhy = 2*x rhy

yblack :: (Double,Double,Double) -> Double
yblack (r,h,y) = y+2*h

blackarea :: (Double,Double,Double) -> Double
blackarea rhy@(r,h,y) = 4*x rhy*(y+h)

x1 :: (Double,Double,Double) -> Double
x1 rhy@(r,h,y) = (-b+sqrt(b^2-4*a*c))/(2*a)
  where
    a = (x rhy)^2 + (y+h)^2
    b = - 2*(x rhy)*y*(y+h)
    c = (x rhy)^2*(y^2-1)

y1 :: (Double,Double,Double) -> Double
y1 rhy = sqrt(1-(x1 rhy)^2)

x2 :: (Double,Double,Double) -> Double
x2 rhy@(r,h,y) = (b+sqrt(b^2-4*a*c))/(2*a)
  where
    a = (x rhy)^2 + (y+h)^2
    b = - 2*(x rhy)*y*(y+h)
    c = (x rhy)^2*(y^2-1)

y2 :: (Double,Double,Double) -> Double
y2 rhy = sqrt(1-(x2 rhy)^2)

y3 :: (Double,Double,Double) -> Double
y3 rhy@(r,h,y) = (y2 rhy - y1 rhy)/(2*r)

x3 :: (Double,Double,Double) -> Double
x3 rhy = sqrt(1-(y3 rhy)^2)

greenarea :: (Double,Double,Double) -> Double
greenarea rhy@(r,h,y) = (x1 rhy)^2*(y+h)/(x rhy)

redok :: (Double,Double,Double) -> Double
redok rhy@(r,h,y) = y^2 - h^2 - (x rhy)^2

bisect :: Double -> (Double -> Double) -> Double -> Double -> Double
bisect epsilon f x0 x1
  | abs(x0 - x1) < epsilon = x
  | y*y0 < 0 = bisect epsilon f x0 x
  | y*y1 < 0 = bisect epsilon f x x1
  | otherwise = error (show ((x0,y0),(x,y),(x1,y1)))
  where
    x = (x0+x1)/2
    (y,y0,y1) = (f x,f x0,f x1)

svg :: (Double,Double,Double) -> String
svg rhy@(r,h,y) =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
    "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" width=\"300\" height=\"300\" viewBox=\"-1.2 -1.2 2.4 2.4\">\n" ++
    "  <polygon points=\"-" ++ show (x1 rhy) ++ "," ++ show (y1 rhy) ++ " " ++ show (x2 rhy) ++ ",-" ++ show (y2 rhy) ++ " -" ++ show (x3 rhy) ++ ",-" ++ show (y3 rhy) ++ "\" stroke=\"yellow\" stroke-width=\"0.01\" fill=\"yellow\"/>\n" ++

    "  <circle cx=\"0\" cy=\"0\" r=\"1\" fill=\"none\" stroke=\"black\" stroke-width=\"0.02\"/>\n" ++
    "  <circle cx=\"0\" cy=\"0\" r=\"0.02\" fill=\"black\"/>\n" ++
    "  <circle cx=\"0\" cy=\"-" ++ show y ++ "\" r=\"0.03\" fill=\"red\"/>\n" ++
    "  <circle cx=\"-" ++ show (x rhy) ++ "\" cy=\"" ++ show h ++ "\" r=\"0.03\" fill=\"red\"/>\n" ++
    "  <circle cx=\"" ++ show (x rhy) ++ "\" cy=\"" ++ show h ++ "\" r=\"0.03\" fill=\"red\"/>\n" ++

    "  <polygon points=\"-" ++ show (xblue rhy) ++ "," ++ show h ++ " " ++ show (xblue rhy) ++ "," ++ show h ++ " " ++ show (x0 rhy) ++ ",-" ++ show (y0 rhy) ++ "\" fill=\"none\" stroke=\"red\" stroke-width=\"0.01\"/>\n" ++

    "  <polygon points=\"-" ++ show (xblack rhy) ++ ",-" ++ show y ++ " " ++ show (xblack rhy) ++ ",-" ++ show y ++ " 0," ++ show (yblack rhy) ++ "\" fill=\"none\" stroke=\"black\" stroke-width=\"0.01\"/>\n" ++

    "  <polygon points=\"0,-" ++ show y ++ " -" ++ show (x1 rhy) ++ "," ++ show (y1 rhy) ++ " " ++ show (x1 rhy) ++ "," ++ show (y1 rhy) ++ "\" fill=\"none\" stroke=\"green\" stroke-width=\"0.02\"/>\n" ++


    "</svg>\n"

wsvg :: (Double,Double,Double) -> IO ()
wsvg rhy = do
    writeFile "/tmp/b.svg" (svg rhy)
    print ("outer",outerarea rhy,"red",redarea rhy,"black",blackarea rhy,"green",greenarea rhy,"redok?",redok rhy)
