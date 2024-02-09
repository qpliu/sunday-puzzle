import Data.Bits(testBit)
import Data.Map(Map,empty,insert,member,fromList,toList,(!))

makeGrids :: (Int,Int) -> [Map (Int,Int) ()]
makeGrids (nx,ny) = [makeGrid i | i <- [0..2^(nx*ny)-1]]
  where
    makeGrid :: Int -> Map (Int,Int) ()
    makeGrid i = fromList [((ix,iy),()) | ix <- [0..nx-1], iy <- [0..ny-1], testBit i (ix+nx*iy)]

validLoop :: (Int,Int) -> Map (Int,Int) () -> Bool
validLoop (nx,ny) grid
  | grid == empty = False
  | hasCornerJoin = False
  | not singleBlob = False
  | otherwise = not hasHole
  where
    hasCornerJoin = or [(member (ix-1,iy+1) grid && not (member (ix-1,iy) grid)) || (member (ix+1,iy+1) grid && not (member (ix+1,iy) grid)) | ix <- [0..nx-1], iy <- [0..ny-1], member (ix,iy) grid, not (member (ix,iy+1) grid)]
    singleBlob = walk empty (map fst $ take 1 $ toList grid) == grid
    walk seen [] = seen
    walk seen ((ix,iy):queue)
      | ix < 0 || iy < 0 || ix >= nx || iy >= ny = walk seen queue
      | member (ix,iy) seen || not (member (ix,iy) grid) = walk seen queue
      | otherwise = walk (insert (ix,iy) () seen) ((ix-1,iy):(ix+1,iy):(ix,iy+1):(ix,iy-1):queue)
    hasHole = or [walkHole empty [(ix,iy)] | ix <- [0..nx-1], iy <- [0..ny-1], not (member (ix,iy) grid)]
    walkHole seen [] = True
    walkHole seen ((ix,iy):queue)
      | ix < 0 || iy < 0 || ix >= nx || iy >= ny = False
      | member (ix,iy) seen || member (ix,iy) grid = walkHole seen queue
      | otherwise = walkHole (insert (ix,iy) () seen) ((ix-1,iy):(ix+1,iy):(ix,iy+1):(ix,iy-1):queue)

getNumbers :: (Int,Int) -> Map (Int,Int) () -> Map (Int,Int) Int
getNumbers (nx,ny) grid = fromList [((ix,iy),getNumber ix iy) | ix <- [0..nx-1], iy <- [0..ny-1]]
  where
    getNumber ix iy
      | member (ix,iy) grid = 4 - neighbors
      | otherwise = neighbors
      where
        neighbors = length [() | i <- [(ix-1,iy),(ix+1,iy),(ix,iy-1),(ix,iy+1)], member i grid]

consistentWith :: (Int,Int) -> Map (Int,Int) Int -> Int -> Map (Int,Int) Int -> Bool
consistentWith (nx,ny) solution index loop =
    and [solution!(ix,iy) == loop!(ix,iy) | ix <- [0..nx-1], iy <- [0..ny-1], testBit index (ix+nx*iy)]

validPuzzles :: (Int,Int) -> [Map (Int,Int) Int] -> Map (Int,Int) Int -> [Int]
validPuzzles (nx,ny) loops solution = [index | index <- [0..2^(nx*ny)-1], length [() | loop <- loops, consistentWith (nx,ny) solution index loop] == 1]

countPuzzles :: (Int,Int) -> Int
countPuzzles (nx,ny) = sum [length $ validPuzzles (nx,ny) loops solution | solution <- loops]
  where
    loops = map (getNumbers (nx,ny)) $ filter (validLoop (nx,ny)) $ makeGrids (nx,ny)

main :: IO ()
main = do
    print $ length $ filter (validLoop (3,3)) $ makeGrids (3,3)
    print $ countPuzzles (3,3)
