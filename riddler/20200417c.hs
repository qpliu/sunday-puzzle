import Data.Array(Array,array,bounds,range,(!))

step :: Array (Integer,Integer) Bool -> Array (Integer,Integer) Bool
step grid = array (bounds grid) (map next (range (bounds grid)))
  where
    ((0,0),(m,n)) = bounds grid
    neighbors (x,y) = length $ filter id $ map (grid!) [(mod (x+1) (m+1),mod (y+1) (n+1)),(x,mod (y+1) (n+1)),(mod (x-1) (m+1),mod (y+1) (n+1)),(mod (x+1) (m+1),y),(mod (x-1) (m+1),y),(mod (x+1) (m+1),mod (y-1) (n+1)),(x,mod (y-1) (n+1)),(mod (x-1) (m+1),mod (y-1) (n+1))]
    next i = (i,neighbors i == 3 || ((grid!i) && neighbors i == 2))

states :: Integer -> [Array (Integer,Integer) Bool]
states n = map makeState [1..(2^(3*n))]
  where
    makeState :: Integer -> Array (Integer,Integer) Bool
    makeState i = array bnds (zip (range bnds) (bits i))
    bnds = ((0,0),(2,n-1))
    bits :: Integer -> [Bool]
    bits 0 = repeat False
    bits x = (mod x 2 /= 0) : bits (div x 2)

returnsWithin :: Int -> Array (Integer,Integer) Bool -> Bool
returnsWithin nsteps grid = r nsteps (step grid)
  where
    r n g | g == grid = n /= nsteps
          | n <= 0 = False
          | otherwise = r (n-1) (step g)

solution :: Array (Integer,Integer) Bool
solution = head $ filter (returnsWithin 10) $ states 5
