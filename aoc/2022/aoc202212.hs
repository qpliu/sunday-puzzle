import Debug.Trace(traceShow)

import Data.Map(Map,toList,(!))
import qualified Data.Map
import Data.Set(Set,difference,elems,fromList,member,singleton,size,union)

parse :: String -> ((Int,Int),(Int,Int),Map (Int,Int) Char)
parse = p (0,0) (0,0) 0 0 []
  where
    p xyS xyE x y heights "" = (xyS,xyE,Data.Map.fromList heights)
    p xyS xyE x y heights (c:cs)
      | c == '\n' = p xyS xyE 0 (y+1) heights cs
      | c == 'S' = p (x,y) xyE (x+1) y (((x,y),'a'):heights) cs
      | c == 'E' = p xyS (x,y) (x+1) y (((x,y),'z'):heights) cs
      | otherwise = p xyS xyE (x+1) y (((x,y),c):heights) cs

moves1 :: Map (Int,Int) Char -> (Int,Int) -> [(Int,Int)]
moves1 heightmap (x,y) = filter valid [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
  where
    elev = heightmap!(x,y)
    valid xy = maybe False (<= (succ elev)) $ Data.Map.lookup xy heightmap

find1 :: ((Int,Int),(Int,Int),Map (Int,Int) Char) -> Maybe Int
find1 (xyS,xyE,heightmap) = search1 xyE heightmap 0 (singleton xyS) (singleton xyS)

search1 :: (Int,Int) -> Map (Int,Int) Char -> Int -> Set (Int,Int) -> Set (Int,Int) -> Maybe Int
search1 xyE heightmap nsteps seen current
  | size current == 0 = Nothing
  | member xyE current = Just nsteps
  | otherwise = search1 xyE heightmap (nsteps+1) (union seen next) (difference next seen)
  where
    next = fromList $ concatMap (moves1 heightmap) $ elems current

testData :: String
testData = unlines [
    "Sabqponm",
    "abcryxxl",
    "accszExk",
    "acctuvwj",
    "abdefghi"
    ]

test :: ()
test
  | (find1 . parse) testData /= Just 31 = error "a"
  | otherwise = ()

part1 :: IO (Maybe Int)
part1 = fmap (find1 . parse) $ readFile "input/12.txt"

run2 :: ((Int,Int),(Int,Int),Map (Int,Int) Char) -> Maybe Int
run2 (_,xyE,heightmap) = minimum $ filter (/= Nothing) $ [find1 (xyS,xyE,heightmap) | xyS <- map fst $ filter ((== 'a') . snd) $ toList heightmap]

test2 :: ()
test2
  | (run2 . parse) testData /= Just 29 = error "a"
  |  otherwise = ()

-- This is slow, but it gets the answer.
part2 :: IO (Maybe Int)
part2 = fmap (run2 . parse) $ readFile "input/12.txt"
