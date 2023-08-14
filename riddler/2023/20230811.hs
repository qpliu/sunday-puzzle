import Data.List(intercalate)

-- define line of sight L as y = Lx, where L = a + sqrt(b) (or a - sqrt(-b))
-- L = LOS a b
data LOS = LOS Rational Rational deriving (Eq,Show)

treeLOSEdges :: Rational -> Int -> Int -> ((LOS,LOS),(Double,Double))
treeLOSEdges r x y = ((los1,los2),(toDouble los1,toDouble los2))
  where
    xx = fromIntegral x
    yy = fromIntegral y
    a = xx*yy/(xx^2-r^2)
    b = r^2/(xx^2-r^2)^2*(xx^2+yy^2-r^2)
    los1 = min (LOS a b) (LOS a (-b))
    los2 = max (LOS a b) (LOS a (-b))

toDouble :: LOS -> Double
toDouble (LOS a b) = fromRational a + signum (fromRational b)*sqrt (fromRational (abs b))

instance Ord LOS where
    compare a b = compare (toDouble a) (toDouble b)

data View = Tree Int Int Double ((LOS,LOS),(Double,Double)) | Gap ((LOS,LOS),(Double,Double)) deriving Show

isGap :: View -> Bool
isGap (Gap _) = True
isGap _ = False

initialGap :: View
initialGap = Gap ((los0,los1),(toDouble los0,toDouble los1))
  where
    los0 = LOS 0 0
    los1 = LOS 1 0

fillView :: Rational -> [View] -> Int -> Int -> [View]
fillView r view x y
  | not (any isGap view) = view
  | y < x = fillView r (concatMap (fillView1 r x y) view) x (y+1)
  | otherwise = fillView r (concatMap (fillView1 r x y) view) (x+1) 0

fillView1 :: Rational -> Int -> Int -> View -> [View]
fillView1 r x y view@(Tree _ _ _ _) = [view]
fillView1 r x y view@(Gap ((gmin,gmax),(gmind,gmaxd)))
  | tmind > gmaxd = [view]
  | tmaxd < gmind = [view]
  | tmind < gmind && tmaxd > gmaxd = [Tree x y dist ((gmin,gmax),(gmind,gmaxd))]
  | tmind < gmind = [Tree x y dist ((gmin,tmax),(gmind,tmaxd)),Gap ((tmax,gmax),(tmaxd,gmaxd))]
  | tmaxd > gmaxd = [Gap ((gmin,tmin),(gmind,tmind)),Tree x y dist ((tmin,gmax),(tmind,gmaxd))]
  | otherwise = [Gap ((gmin,tmin),(gmind,tmind)),Tree x y dist ((tmin,tmax),(tmind,tmaxd)),Gap ((tmax,gmax),(tmaxd,gmaxd))]
  where 
    ((tmin,tmax),(tmind,tmaxd)) = treeLOSEdges r x y
    dist = sqrt (fromIntegral (x^2) + fromIntegral (y^2))

main :: IO ()
main = do
    mapM_ print $ fillView (1/4) [initialGap] 1 0
    putStrLn ""
    mapM_ print $ fillView (1/10) [initialGap] 1 0

viz :: Int -> Int -> Rational -> String
viz size duration r =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
    "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\"" ++
    " width=\"" ++ show size ++ "\" height=\"" ++ show size ++ "\"" ++
    " viewBox=\"-" ++ show (xmax+1) ++ " -" ++ show (xmax+1) ++
    " " ++ show (2*xmax+2) ++ " " ++ show (2*xmax+2) ++ "\">\n" ++
    "<polygon fill=\"#ffd\" points=\"" ++ intercalate " " (map (\ (x,y) -> show x ++ "," ++ show y) endpoints8) ++ "\"/>\n" ++
    concat longestLines ++
    "<line x1=\"0\" y1=\"0\" stroke=\"blue\" stroke-width=\"0.01\">\n" ++
    "<animate attributeName=\"x2\" dur=\"" ++ show duration ++ "s\" repeatCount=\"indefinite\"" ++
    " values=\"" ++ intercalate ";" (map (show . fst) endpoints8) ++ "\"/>" ++
    "<animate attributeName=\"y2\" dur=\"" ++ show duration ++ "s\" repeatCount=\"indefinite\"" ++
    " values=\"" ++ intercalate ";" (map (show . snd) endpoints8) ++ "\"/>" ++
    "</line>\n" ++
    concat circles ++
    concat (hilightCircles 0 circles8) ++
    "</svg>\n"
  where
    rd = fromRational r :: Double
    view = fillView r [initialGap] 1 0
    xmax = maximum (map (\ (Tree x _ _ _) -> x) view)
    visible x y = any (\ (Tree xx yy _ _) -> (xx == abs x && yy == abs y) || (xx == abs y && yy == abs x)) view
    circles = ["<circle cx=\"" ++ show x ++ "\"" ++
               " cy=\"" ++ show y ++ "\"" ++
               " r=\"" ++ show rd ++ "\"" ++
               " fill=\"" ++ (if visible x y then "black" else "#ddd") ++ "\"/>\n"
               | x <- [-xmax-1 .. xmax+1], y <- [-xmax-1 .. xmax+1], (x,y) /= (0,0)]
    toEndpoints (Tree x y _ (_,(los1,los2))) =
        [(x1^2+y1^2,(x1,y1)),(x2^2+y2^2,(x2,y2))]
      where
        xx = fromIntegral x
        yy = fromIntegral y
        x1 = (xx+los1*yy - sqrt (abs ((xx+los1*yy)^2 - (1+los1^2)*(xx^2+yy^2-rd^2))))/(1+los1^2)
        y1 = los1*x1
        x2 = (xx+los2*yy - sqrt (abs ((xx+los2*yy)^2 - (1+los2^2)*(xx^2+yy^2-rd^2))))/(1+los2^2)
        y2 = los2*x2
    endpoints = concatMap toEndpoints view
    endpoints8 = map snd endpoints ++
        map (\ (_,(x,y)) -> (y,x)) (drop 1 $ reverse endpoints) ++
        map (\ (_,(x,y)) -> (-y,x)) (drop 1 endpoints) ++
        map (\ (_,(x,y)) -> (-x,y)) (drop 1 $ reverse endpoints) ++
        map (\ (_,(x,y)) -> (-x,-y)) (drop 1 $ endpoints) ++
        map (\ (_,(x,y)) -> (-y,-x)) (drop 1 $ reverse endpoints) ++
        map (\ (_,(x,y)) -> (y,-x)) (drop 1 $ endpoints) ++
        map (\ (_,(x,y)) -> (x,-y)) (drop 1 $ reverse endpoints)
    (_,(longestx,longesty)) = maximum endpoints
    longestLines = ["<line x1=\"0\" y1=\"0\" x2=\"" ++ show x ++ "\" y2=\"" ++ show y ++ "\" stroke=\"red\" stroke-width=\"0.02\"/>\n" | (x,y) <- [(longestx,longesty),(longesty,longestx),(-longesty,longestx),(-longestx,longesty),(-longestx,-longesty),(-longesty,-longestx),(longesty,-longestx),(longestx,-longesty)]]
    circles8 =
        map (\ (Tree x y _ _) -> (x,y)) view ++
        map (\ (Tree x y _ _) -> (y,x)) (drop 1 $ reverse view) ++
        map (\ (Tree x y _ _) -> (-y,x)) (drop 1 view) ++
        map (\ (Tree x y _ _) -> (-x,y)) (drop 1 $ reverse view) ++
        map (\ (Tree x y _ _) -> (-x,-y)) (drop 1 view) ++
        map (\ (Tree x y _ _) -> (-y,-x)) (drop 1 $ reverse view) ++
        map (\ (Tree x y _ _) -> (y,-x)) (drop 1 view) ++
        map (\ (Tree x y _ _) -> (x,-y)) (drop 1 $ reverse view)
    hilightCircles _ [] = []
    hilightCircles i ((x,y):xys) =
        ("<circle cx=\"" ++ show x ++ "\"" ++
         " cy=\"" ++ show y ++ "\"" ++
         " r=\"" ++ show rd ++ "\"" ++
         " fill=\"#ccf\">" ++
         "<animate" ++
         " attributeName=\"fill-opacity\" dur=\"" ++ show duration ++ "s\" repeatCount=\"indefinite\"" ++
         " values=\"" ++ intercalate ";" (take i (repeat "0") ++ "1" : take (length xys) (repeat "0")) ++ "\"/>" ++
         "</circle>\n") : hilightCircles (i+1) xys
