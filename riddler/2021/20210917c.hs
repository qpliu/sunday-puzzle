import Data.Map(Map,alter,empty,fromList,toList)

bruteForce :: Integer -> Double
bruteForce n = product [r*(cos (2*pi*fromIntegral k/fromIntegral n) - cos (2*pi*fromIntegral (k+1)/fromIntegral n)) | k <- [0 .. n `div` 2 - 1]]
  where r = 1/ sin (pi/fromIntegral n)

type Poly = Map Integer Integer

mult :: Poly -> Poly -> Poly
mult a b = foldl collect empty [(aexp+bexp,acoef*bcoef) | (aexp,acoef) <- toList a, (bexp,bcoef) <- toList b]
  where
    collect poly (exp,coef) = alter (Just . maybe coef (+coef)) exp poly

prod :: [Poly] -> Poly
prod polys = foldl mult one polys
  where one = fromList [(0,1)]

poly :: Integer -> Double
poly n = r^(n `div` 2)*sum [subst exp coef | (exp,coef) <- toList p]
  where
    -- either there's something wrong with this code or something's
    -- wrong with the reasoning behind it, because it disagrees with
    -- the brute force calculation for n >= 4
    p = prod [fromList [(k,1),(-k,1),(k+1,-1),(-k-1,-1)] | k <- [0 .. n `div` 2 - 1]]
    r = 1 / sin (pi/fromIntegral n)
    subst exp coef
      | exp < 0 = 0
      | otherwise = fromIntegral coef*cos (2*pi*fromIntegral exp/fromIntegral n)

main :: IO ()
main = do
    print (bruteForce 1000)
    print (bruteForce 1001)
