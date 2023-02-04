import Data.Map(Map,findWithDefault,fromList,mapKeys,mapWithKey,toList)

parse :: String -> Map (Int,Int,Int) Int
parse = fromList . p 0 0
  where
    p x y ('\n':rest) = p 0 (y+1) rest
    p x y ('#':rest) = ((x,y,0),1) : p (x+1) y rest
    p x y (_:rest) = p (x+1) y rest
    p _ _ [] = []

step :: Map (Int,Int,Int) Int -> Map (Int,Int,Int) Int
step grid =
    mapWithKey nextState $ fromList $ concatMap withNeighbors $ toList grid
  where
    nextState (x,y,z) _
      | neighbors == 2 = findWithDefault 0 (x,y,z) grid
      | neighbors == 3 = 1
      | otherwise = 0
      where
        neighbors = sum [findWithDefault 0 (x+dx,y+dy,z+dz) grid | dx <- [-1,0,1], dy <- [-1,0,1], dz <- [-1,0,1], dx /= 0 || dy /= 0 || dz /= 0]
    withNeighbors ((x,y,z),1) = [((x+dx,y+dy,z+dz),0) | dx <- [-1,0,1], dy <- [-1,0,1], dz <- [-1,0,1]]
    withNeighbors _ = []

testData :: String
testData = ".#.\n..#\n###\n"

test :: ()
test
  | (sum . head . drop 6 . iterate step . parse) testData /= 112 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . head . drop 6 . iterate step . parse) $ readFile "input/17.txt"

to4d :: Map (Int,Int,Int) Int -> Map (Int,Int,Int,Int) Int
to4d = mapKeys (\ (x,y,z) -> (x,y,z,0))

step2 :: Map (Int,Int,Int,Int) Int -> Map (Int,Int,Int,Int) Int
step2 grid =
    mapWithKey nextState $ fromList $ concatMap withNeighbors $ toList grid
  where
    nextState (x,y,z,w) _
      | neighbors == 2 = findWithDefault 0 (x,y,z,w) grid
      | neighbors == 3 = 1
      | otherwise = 0
      where
        neighbors = sum [findWithDefault 0 (x+dx,y+dy,z+dz,w+dw) grid | dx <- [-1,0,1], dy <- [-1,0,1], dz <- [-1,0,1], dw <- [-1,0,1], dx /= 0 || dy /= 0 || dz /= 0 || dw /= 0]
    withNeighbors ((x,y,z,w),1) = [((x+dx,y+dy,z+dz,w+dw),0) | dx <- [-1,0,1], dy <- [-1,0,1], dz <- [-1,0,1], dw <- [-1,0,1]]
    withNeighbors _ = []

test2 :: ()
test2
  | (sum . head . drop 6 . iterate step2 . to4d . parse) testData /= 848 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (sum . head . drop 6 . iterate step2 . to4d . parse) $ readFile "input/17.txt"
