import Data.Map(Map,alter,empty,fromList,mapKeys,toList,unionsWith)

p :: Map Int Integer -> Map Int Integer -> Map Int Integer
p p1 p2 = unionsWith (+) (mapKeys (+1) p1 : mapKeys (+1) p2 : [fromList [(k1+k2+2,-c1*c2)] | (k1,c1) <- toList p1, (k2,c2) <- toList p2])

cNextUpper :: [Map Int Integer] -> [Map Int Integer]
cNextUpper pRow = zipWith p (empty:pRow) (pRow ++ [empty])

cNextLower :: [Map Int Integer] -> [Map Int Integer]
cNextLower pRow = zipWith p pRow (tail pRow)

cTop :: [Map Int Integer]
cTop = [fromList [(0,1)]]

cBottom :: Int -> Map Int Integer
cBottom n = (head . head . drop (n-1) . iterate cNextLower . head . drop (n-1) . iterate cNextUpper) cTop

probability :: Rational -> Map Int Integer -> Rational
probability s cbottom = sum [s^k*fromIntegral c | (k,c) <- toList cbottom]

ps :: Fractional f => f -> [(Int,f)]
ps s = upper [1]
  where
    nextP s p1 p2 = s*p1 + s*p2 - s*s*p1*p2
    upper plist = (length plist,lower plist) : upper (zipWith (nextP s) (0:plist) (plist++[0]))
    lower [p] = p
    lower plist = lower (zipWith (nextP s) plist (tail plist))

ulimit :: Fractional f => f -> f
ulimit s = (1-(1-s)^2)^2

main :: IO ()
main = do
    mapM_ print $ take 10 $ zip (ps 0.5) (ps 0.7)
    mapM_ print $ take 3 $ drop 100 $ zip (ps 0.5) (ps 0.7)
    mapM_ print $ take 3 $ drop 1000 $ zip (ps 0.5) (ps 0.7)
    print (ulimit 0.5,ulimit 0.7)
    mapM_ (print . f) [1..6]
    mapM_ (print . cBottom) [1..4]
  where
    f n = (n,fromRational mag :: Double,fromRational fos :: Double)
      where
        cbottom = cBottom n
        mag = probability (1/2) cbottom
        fos = probability (7/10) cbottom
