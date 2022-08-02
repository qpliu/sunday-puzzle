import Data.Set(Set,fromList,member,size,toList)
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

cWithDx :: Double -> Double -> Double
cWithDx dx r = integrate (-1) 1 f dx / sqrt (2*pi)
  where f x = exp(-(x+r)^2/2)*erf(sqrt(max 0 (1-x^2)/2))

c :: Double -> Double
c = cWithDx 0.001

xy :: (Int,Int) -> (Double,Double)
xy (i,j) = (fromIntegral (2*i) + fromIntegral (j `mod` 2),fromIntegral j * sqrt 3)

r :: (Double,Double) -> (Int,Int) -> Double
r (x0,y0) ij = sqrt ((x-x0)^2 + (y-y0)^2)
  where (x,y) = xy ij

showTiling :: Set (Int,Int) -> String
showTiling set = (unlines . tail . concat) [[line1 j, line2 j] | j <- [jmax+1,jmax..jmin]]
  where
    line1 j = (if j `mod` 2 == 0 then "|" else "  |") ++ concat [" " ++ if (i,j) == (0,0) then "0 |" else if member (i,j) set then "O |" else "  |" | i <- [imin..imax]]
    line2 j = (if j `mod` 2 == 0 then "" else " /") ++ (concat . take (imax-imin+1) . repeat) " \\ /"
    imin = minimum (Data.Set.map fst set)
    imax = maximum (Data.Set.map fst set)
    jmin = minimum (Data.Set.map snd set)
    jmax = maximum (Data.Set.map snd set)

center :: ((Double,Double) -> Double) -> Set (Int,Int) -> (Double,Double)
center weight set = (sum (zipWith (*) (map fst rs) weights)/sum weights,sum (zipWith (*) (map snd rs) weights)/sum weights)
  where
    rs = map xy (toList set)
    weights = map weight rs

cups :: [Set (Int,Int)]
cups = [fromList [(0,0)],
        fromList [(0,0),(1,0)], -- 2
        fromList [(0,0),(1,0),(0,1)], -- 3
        fromList [(0,0),(1,0),(0,1),(0,-1)], -- 4
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1)], -- 5
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1)], -- 6
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(1,1)], -- 6
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0)], -- 7
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2)], -- 8
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(0,-2)], -- 9
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(1,-1)], -- 9
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(1,1),(1,-1)], -- 9
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(-2,-1),(1,-1)], -- 10
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,-2),(-2,-1),(1,-1)], -- 10
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(1,-1),(1,1),(2,0)], -- 10
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,-2),(-2,-1),(-1,-2),(1,-2)], -- 11
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(-2,1),(1,-1),(0,-2)], -- 11
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(1,-2),(1,-1),(0,-2)], -- 11
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(-1,2),(1,-1),(0,-2)], -- 11
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(1,-2),(1,1),(1,-1),(0,-2)], -- 12
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(-2,1),(1,-1),(0,-2),(-1,-2)], -- 13
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(1,-2),(-2,-1),(1,-1),(0,-2),(-1,-2)], -- 13
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(-2,-1),(1,-1),(-2,1),(1,1),(0,-2)], -- 13
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(1,-2),(-2,-1),(1,-1),(0,-2),(-1,-2),(0,2)], -- 13
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(-2,-1),(1,-1),(-2,1),(1,1),(0,-2),(2,0)], -- 14
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,-2),(-2,-1),(1,-1),(-2,1),(1,1),(-1,-2),(1,-2)], -- 14
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(1,-2),(1,-1),(1,2),(1,1),(0,-2),(2,0)], -- 14
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(-2,-1),(1,-1),(-2,1),(1,1),(0,-2),(2,0),(-2,0)], -- 15
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(1,-2),(1,-1),(1,2),(1,1),(0,-2),(2,0),(2,1)], -- 15
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(-2,-1),(1,-1),(-2,1),(1,1),(0,-2),(2,0),(-1,2),(-1,-2)], -- 16
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(1,-2),(1,-1),(1,2),(1,1),(0,-2),(2,0),(2,1),(2,-1)], -- 16
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(1,-2),(1,-1),(1,2),(1,1),(0,-2),(2,0),(2,1),(-2,-1)], -- 16
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(-2,-1),(1,-1),(-2,1),(1,1),(0,-2),(2,0),(-1,2),(-1,-2),(-2,0)], -- 17
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(1,-2),(1,-1),(1,2),(1,1),(0,-2),(2,0),(2,1),(-2,-1),(2,-1)], -- 17
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(1,-2),(1,-1),(1,2),(1,1),(0,-2),(2,0),(2,1),(-2,0),(2,-1)], -- 17
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(-2,-1),(1,-1),(-2,1),(1,1),(0,-2),(2,0),(-1,2),(-1,-2),(-2,0),(1,2)], -- 18
        fromList [(0,0),(1,0),(0,1),(0,-1),(-1,1),(-1,-1),(-1,0),(0,2),(-2,-1),(1,-1),(-2,1),(1,1),(0,-2),(2,0),(-1,2),(-1,-2),(-2,0),(1,2),(1,-2)] -- 19
       ]

chance :: Set (Int,Int) -> (Double,Double) -> Double
chance set (x0,y0) = (sum . map (c . r (x0,y0)) . toList) set

findCenter :: Double -> Set (Int,Int) -> (Double,Double)
findCenter delta set = iterateR (center (const 1) set)
  where
    iterateR (x1,y1)
      | (x1-x2)^2 + (y1-y2)^2 < delta^2 = (x2,y2)
      | otherwise = iterateR (x2,y2)
      where (x2,y2) = center (\ (x,y) -> c (sqrt ((x-x1)^2 + (y-y1)^2))) set

printItem :: Set (Int,Int) -> IO ()
printItem set = do
    print (size set,ctr,chance set ctr)
    putStr (showTiling set)
  where ctr = findCenter 0.001 set

main :: IO ()
main = mapM_ printItem cups
