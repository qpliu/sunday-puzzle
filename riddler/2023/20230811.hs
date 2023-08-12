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
