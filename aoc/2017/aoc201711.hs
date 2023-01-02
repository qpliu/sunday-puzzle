{-
--- Day 11: Hex Ed ---

Crossing the bridge, you've barely reached the other side of the stream when a
program comes up to you, clearly in distress. "It's my child process," she
says, "he's gotten lost in an infinite grid!"

Fortunately for her, you have plenty of experience with infinite grids.

Unfortunately for you, it's a hex grid.

The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be
found to the north, northeast, southeast, south, southwest, and northwest:

|   \ n  /
| nw +--+ ne
|   /    \
| -+      +-
|   \    /
| sw +--+ se
|   / s  \

You have the path the child process took. Starting where he started, you need
to determine the fewest number of steps required to reach him. (A "step" means
to move from the hex you are in to any adjacent hex.)

For example:

 - ne,ne,ne is 3 steps away.
 - ne,ne,sw,sw is 0 steps away (back where you started).
 - ne,ne,s,s is 2 steps away (se,se).
 - se,sw,se,sw,sw is 3 steps away (s,s,sw).
-}

import Data.List(groupBy)

parse :: String -> [String]
parse = groupBy (\ a b -> (a == 'n' || a == 's') && (b == 'e' || b == 'w'))

step :: (Int,Int) -> String -> (Int,Int)
step (x,y) "n" = (x,y+1)
step (x,y) "ne" | even x = (x+1,y) | otherwise = (x+1,y+1)
step (x,y) "se" | even x = (x+1,y-1) | otherwise = (x+1,y)
step (x,y) "s" = (x,y-1)
step (x,y) "sw" | even x = (x-1,y-1) | otherwise = (x-1,y)
step (x,y) "nw" | even x = (x-1,y) | otherwise = (x-1,y+1)
step (x,y) _ = (x,y)

hex :: String -> (Int,Int)
hex = foldl step (0,0) . parse

dist :: (Int,Int) -> Int
dist (x,y)
  | x < 0 = dist (-x,y)
  | y >= 0 = x + max 0 (y - x `div` 2)
  | otherwise = x + max 0 (-1 - y - x `div` 2)

test :: ()
test
  | dist (hex "ne,ne,ne") /= 3 = error "a"
  | dist (hex "ne,ne,sw,sw") /= 0 = error "b"
  | dist (hex "ne,ne,s,s") /= 2 = error "c"
  | hex "ne,ne,s,s" /= hex "se,se" = error "d"
  | dist (hex "se,sw,se,sw,sw") /= 3 = error "e"
  | hex "se,sw,se,sw,sw" /= hex "s,s,sw" = error "f"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (dist . hex) $ readFile "input/11.txt"

step2 :: (Int,(Int,Int)) -> String -> (Int,(Int,Int))
step2 (maxdist,xy) move = (max maxdist (dist newxy),newxy)
  where newxy = step xy move

hex2 :: String -> (Int,(Int,Int))
hex2 = foldl step2 (0,(0,0)) . parse

part2 :: IO Int
part2 = fmap (fst . hex2) $ readFile "input/11.txt"
