import Data.List(intercalate)
import Data.Map(Map,alter,empty,fromList,toList,(!))
import qualified Data.Set

erf :: Double -> Double
erf x = sign*y 
    where
        a1 =  0.254829592
        a2 = -0.284496736
        a3 =  1.421413741
        a4 = -1.453152027
        a5 =  1.061405429
        p  =  0.3275911

        -- Abramowitz and Stegun formula 7.1.26
        sign = if x > 0
                   then  1
                   else -1
        t  =  1.0/(1.0 + p* abs x)
        y  =  1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x)

integrate :: Double -> Double -> (Double -> Double) -> Double -> Double
integrate x0 x1 f dx = dx*sum [f x | x <- [x0,x0+dx .. x1]]

c :: Double -> Double -> Double
c dx r = integrate (-1) 1 f dx / sqrt (2*pi)
  where f x = exp(-(x+r)^2/2)*erf(sqrt(max 0 (1-x^2)/2))

xy :: (Int,Int) -> (Double,Double)
xy (i,j) = (fromIntegral (2*i) + fromIntegral (j `mod` 2),fromIntegral j * sqrt 3)

coordName :: (Int,Int) -> String
coordName (i,j) = "(" ++ show (2*i+(j`mod`2)) ++ "," ++ show j ++ "*sqrt3)"

data Center = One | Two | Three deriving (Eq,Ord,Show)

r :: Center -> (Int,Int) -> Double
r center ij = sqrt ((x-x0)^2 + (y-y0)^2)
  where
    (x,y) = xy ij
    (x0,y0) = case center of { One -> (0,0); Two -> (1,0); Three -> (1,1/sqrt 3) }

rName :: Center -> (Int,Int) -> String
rName center (i,j) =
    case center of
      One -> "sqrt(" ++ show (x^2 + 3*j^2) ++ ")"
      Two -> "sqrt(" ++ show ((x-1)^2 + 3*j^2) ++ ")"
      Three -> "sqrt(" ++ show (3*((x-1)^2 + 3*j^2-2*j) + 1) ++ "/3)"
  where
    x = 2*i + j`mod`2

cups :: Center -> [(Int,Int)]
cups center = (0,0) : next 0 0 0 0 (Data.Set.fromList [(0,0)])
  where
    next mini minj maxi maxj set = (nexti,nextj) : next (min mini nexti) (min minj nextj) (max maxi nexti) (max maxj nextj) (Data.Set.insert (nexti,nextj) set)
      where (_,(nexti,nextj)) = minimum [(r center (i,j),(i,j)) | i <- [mini-1..maxi+1], j <- [minj-1..maxj+1], not ((i,j) `Data.Set.member` set)]

cn :: Int -> (String,(Double,Center,[(Int,Int)]))
cn n = addName $ maximum [(sum [c 0.001 (r center ij)| ij <- take n (cups center)],center,take n (cups center)) | center <- [One,Two,Three]]
  where
    addName ans@(_,center,ijs) = (show n ++ ":" ++ intercalate "+" [show n++"C("++name++")"|(name,n) <- toList names],ans)
      where
        names = foldr (alter (Just . maybe 1 (+1))) empty (map (rName center) ijs)

main :: IO ()
main = do
  print ("C(0)",c 0.001 0)
  print ("C(1)",c 0.001 1)
  print ("C(2/sqrt3)",c 0.001 (2/sqrt 3))
  print ("C(sqrt2)",c 0.001 (sqrt 2))
  print ("C(sqrt3)",c 0.001 (sqrt 3))
  print ("C(2)",c 0.001 2)
  print ("C(4/sqrt3)",c 0.001 (4/sqrt 3))
  print ("C(0)+3C(2)",c 0.001 0 + 3*c 0.001 2)
  print ("2C(1)+2C(sqrt3)",2*c 0.001 1 + 2*c 0.001 (sqrt 3))
  print ("4*C(sqrt2)",4*c 0.001 (sqrt 2))
  print ("3*C(2/sqrt3)+C(4/sqrt3)",3*c 0.001 (2/sqrt 3) + c 0.001 (4/sqrt 3))
  mapM_ (print . cn) [1..20]
