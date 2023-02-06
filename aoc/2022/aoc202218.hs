import Data.Set(Set,difference,elems,empty,fold,fromList,intersection,member,singleton,size,union)
import qualified Data.Set

parse :: String -> Set (Int,Int,Int)
parse = fromList . map (read . ("("++) . (++")")) . lines

countFaces :: Set (Int,Int,Int) -> Int
countFaces cubes = sum $ map sides $ elems cubes
  where
    sides (x,y,z) =
          (if member (x+1,y,z) cubes then 0 else 1)
        + (if member (x-1,y,z) cubes then 0 else 1)
        + (if member (x,y+1,z) cubes then 0 else 1)
        + (if member (x,y-1,z) cubes then 0 else 1)
        + (if member (x,y,z-1) cubes then 0 else 1)
        + (if member (x,y,z+1) cubes then 0 else 1)

testData :: String
testData = unlines [
    "2,2,2",
    "1,2,2",
    "3,2,2",
    "2,1,2",
    "2,3,2",
    "2,2,1",
    "2,2,3",
    "2,2,4",
    "2,2,6",
    "1,2,5",
    "3,2,5",
    "2,1,5",
    "2,3,5"
    ]

test :: ()
test
  | (countFaces . parse) testData /= 64 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (countFaces . parse) $ readFile "input/18.txt"

getBounds :: Set (Int,Int,Int) -> ((Int,Int,Int),(Int,Int,Int))
getBounds cubes = ((minimum xs,minimum ys,minimum zs),(maximum xs,maximum ys,maximum zs))
  where
    xs = Data.Set.map (\ (x,y,z) -> x) cubes
    ys = Data.Set.map (\ (x,y,z) -> y) cubes
    zs = Data.Set.map (\ (x,y,z) -> z) cubes

neighbors :: Set (Int,Int,Int) -> (Int,Int,Int) -> [(Int,Int,Int)]
neighbors exclusions (x,y,z) = filter (not . (`member` exclusions)) [(x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z),(x,y,z+1),(x,y,z-1)]

getBordering :: Set (Int,Int,Int) -> Set (Int,Int,Int)
getBordering cubes = fromList $ concatMap (neighbors cubes) $ elems cubes

classify :: Set (Int,Int,Int) -> (Int,Int,Int) -> (Set (Int,Int,Int),Set (Int,Int,Int)) ->  (Set (Int,Int,Int),Set (Int,Int,Int))
classify cubes point (exterior,interior)
  | member point cubes = error "mistake"
  | member point exterior || member point interior = (exterior,interior)
  | otherwise = walk (singleton point) (singleton point)
  where
    ((xmin,ymin,zmin),(xmax,ymax,zmax)) = getBounds cubes
    outOfBounds (x,y,z) = x < xmin || y < ymin || z < zmin || x > xmax || y > ymax || z > zmax

    walk seen current
      | any outOfBounds current || size (intersection current exterior) > 0 = (union seen exterior,interior)
      | size current == 0 || size (intersection current interior) > 0 = (exterior,union seen interior)
      | otherwise = walk (union next seen) (difference next seen)
      where
        next = fromList $ concatMap (neighbors cubes) $ elems current

classifyBorder :: Set (Int,Int,Int) -> (Set (Int,Int,Int),Set (Int,Int,Int))
classifyBorder cubes = fold (classify cubes) (empty,empty) $ getBordering cubes

countExteriorFaces :: Set (Int,Int,Int) -> (Set (Int,Int,Int),Set (Int,Int,Int)) -> Int
countExteriorFaces cubes (exterior,interior) = sum $ map sides $ elems cubes
  where
    exclusions = union cubes interior
    sides xyz = length $ neighbors exclusions xyz

run2 :: String -> Int
run2 input = countExteriorFaces cubes classified
  where
    cubes = parse input
    classified = classifyBorder cubes

test2 :: ()
test2
  | run2 testData /= 58 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap run2 $ readFile "input/18.txt"
