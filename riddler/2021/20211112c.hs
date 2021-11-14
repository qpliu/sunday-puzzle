w :: (Rational -> Rational -> Rational) -> Rational -> Rational -> Rational
w strategy stickLength geniesMinimumStickLength
  | f <= 2*g || t*f <= g = (f-g)^2/(4*(1-g))
  | t <= 0 || t >= 1 = error ("invalid strategy: t=" ++ show t ++ ", f=" ++ show f ++ ", g=" ++ show g)
  | t >= 1/2 = (t*f-g)/(1-g) * (1-t)*f
  | otherwise = (t*f-g)/(1-g) * (1-t)*f + (1 - (t*f-g)/(1-g))*w strategy ((1-t)*f) (t*f)
  where
    t = strategy f g
    f = stickLength
    g = geniesMinimumStickLength

main :: IO ()
main = do
    mapM_ print [let x = w (strategy fraction) 1 0 in (fraction,x,fromRational x :: Double) | fraction <- [1/30,2/30..11/30]]
  where
    strategy d f g = (d+g)/f
