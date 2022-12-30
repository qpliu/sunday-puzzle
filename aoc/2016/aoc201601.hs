{-
--- Day 1: No Time for a Taxicab ---

Santa's sleigh uses a very high-precision clock to guide its movements, and the
clock's oscillator is regulated by stars. Unfortunately, the stars have been
stolen... by the Easter Bunny. To save Christmas, Santa needs you to retrieve
all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each
day in the Advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near",
unfortunately, is as close as you can get - the instructions on the Easter
Bunny Recruiting Document the Elves intercepted start here, and nobody had time
to work them out further.

The Document indicates that you should start at the given coordinates (where
you just landed) and face North. Then, follow the provided sequence: either
turn left (L) or right (R) 90 degrees, then walk forward the given number of
blocks, ending at a new intersection.

There's no time to follow such ridiculous instructions on foot, though, so you
take a moment and work out the destination. Given that you can only walk on the
street grid of the city, how far is the shortest path to the destination?

For example:

 - Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks
   away.
 - R2, R2, R2 leaves you 2 blocks due South of your starting position, which is
   2 blocks away.
 - R5, L5, R5, R3 leaves you 12 blocks away.

How many blocks away is Easter Bunny HQ?
-}

import Data.Char(isDigit)
import Data.Set(Set,empty,insert,member)

follow1 :: ((Int,Int),(Int,Int)) -> String -> ((Int,Int),(Int,Int))
follow1 ((dx,dy),(x,y)) (turn:steps) = ((newdx,newdy),(x+n*newdx,y+n*newdy))
  where
    n = read $ takeWhile isDigit steps
    (newdx,newdy) | turn == 'L' = (-dy,dx) | otherwise = (dy,-dx)

follow :: [String] -> (Int,Int)
follow insns = snd $ foldl follow1 ((0,1),(0,0)) insns

distance :: (Int,Int) -> Int
distance (x,y) = abs x+abs y

test :: ()
test
  | follow ["R2","L3"] /= (2,3) = error "a"
  | distance (follow ["R2","L3"]) /= 5 = error "b"
  | follow ["R2","R2","R2"] /= (0,-2) = error "c"
  | distance (follow ["R2","R2","R2"]) /= 2 = error "d"
  | distance (follow ["R5","L5","R5","R3"]) /= 12 = error "e"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (distance . follow . words) $ readFile "input/01.txt"

part2follow1 :: (Set (Int,Int),((Int,Int),(Int,Int))) -> String -> Either (Int,Int) (Set (Int,Int),((Int,Int),(Int,Int)))
part2follow1 (visited,((dx,dy),(x,y))) (turn:steps) = f1 visited (x,y) n
  where
    n = read $ takeWhile isDigit steps
    (newdx,newdy) | turn == 'L' = (-dy,dx) | otherwise = (dy,-dx)
    f1 v (newx,newy) i
      | (newx,newy) `member` visited = Left (newx,newy)
      | i <= 0 = Right (v,((newdx,newdy),(newx,newy)))
      | otherwise = f1 (insert (newx,newy) v) (newx+newdx,newy+newdy) (i-1)

part2follow :: [String] -> (Int,Int)
part2follow insns = f insns (empty,((0,1),(0,0)))
  where
    f insns state = either id (f (tail insns)) (part2follow1 state (head insns))

test2 :: ()
test2
  | part2follow (words "R8, R4, R4, R8") /= (4,0) = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (distance . part2follow . words) $ readFile "input/01.txt"
