{-
--- Day 9: Smoke Basin ---

These caves seem to be lava tubes. Parts are even still volcanically active;
small hydrothermal vents release smoke into the caves that slowly settles like
rain.

If you can model how the smoke flows through the caves, you might be able to
avoid it and be that much safer. The submarine generates a heightmap of the
floor of the nearby caves for you (your puzzle input).

Smoke flows to the lowest point of the area it's in. For example, consider the
following heightmap:

| 2199943210
| 3987894921
| 9856789892
| 8767896789
| 9899965678

Each number corresponds to the height of a particular location, where 9 is the
highest and 0 is the lowest a location can be.

Your first goal is to find the low points - the locations that are lower than
any of its adjacent locations. Most locations have four adjacent locations (up,
down, left, and right); locations on the edge or corner of the map have three
or two adjacent locations, respectively. (Diagonal locations do not count as
adjacent.)

In the above example, there are four low points, all highlighted: two are in
the first row (a 1 and a 0), one is in the third row (a 5), and one is in the
bottom row (also a 5). All other locations on the heightmap have some lower
adjacent location, and so are not low points.

The risk level of a low point is 1 plus its height. In the above example, the
risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels of
all low points in the heightmap is therefore 15.

Find all of the low points on your heightmap. What is the sum of the risk
levels of all low points on your heightmap?
-}

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
