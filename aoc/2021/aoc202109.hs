import Data.Char(ord)
import Data.List(sort)
import Data.Map(Map,findWithDefault,fromList,toList)
import Data.Set(Set,empty,insert,member,size)

parse :: String -> Map (Int,Int) Int
parse = fromList . p 0 0
  where
    p x y ('\n':rest) = p 0 (y+1) rest
    p x y (ch:rest) = ((x,y),ord ch - ord '0') : p (x+1) y rest
    p _ _ _ = []

lowPoints :: Map (Int,Int) Int -> [((Int,Int),Int)]
lowPoints heights = filter isLowPoint $ toList heights
  where
    isLowPoint ((x,y),h) = and [h < findWithDefault 10 (x+dx,y+dy) heights | (dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)]]

testData :: String
testData = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678\n"

test :: ()
test
  | (sum . map ((+1) . snd) . lowPoints . parse) testData /= 15 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . map ((+1) . snd) . lowPoints . parse) $ readFile "input/09.txt"

mapBasin :: Map (Int,Int) Int -> (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
mapBasin heights xy@(x,y) basin
  | findWithDefault 9 xy heights >= 9 = basin
  | member xy basin = basin
  | otherwise = foldr (mapBasin heights) (insert xy basin) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

mapBasins :: Map (Int,Int) Int -> ((Int,Int),Int) -> [Set (Int,Int)] -> [Set (Int,Int)]
mapBasins heights (xy,height) basins
  | height >= 9 = basins
  | any (member xy) basins = basins
  | otherwise = mapBasin heights xy empty:basins

run2 :: String -> Int
run2 input = product $ take 3 $ reverse $ sort $ map size $ foldr (mapBasins heights) [] $ toList heights
  where heights = parse input

test2 :: ()
test2
  | run2 testData /= 1134 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap run2 $ readFile "input/09.txt"
