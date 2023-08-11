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
