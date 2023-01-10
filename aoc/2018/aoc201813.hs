{-
-- Day 13: Mine Cart Madness ---

A crop of this size requires significant logistics to transport produce, soil,
fertilizer, and so on. The Elves are very busy pushing things around in carts
on some kind of rudimentary system of tracks they've come up with.

Seeing as how cart-and-track systems don't appear in recorded history for
another 1000 years, the Elves seem to be making this up as they go along. They
haven't even figured out how to avoid collisions yet.

You map out the tracks (your puzzle input) and see where you can help.

Tracks consist of straight paths (| and -), curves (/ and \), and intersections
(+). Curves connect exactly two perpendicular pieces of track; for example,
this is a closed loop:

| /----\
| |    |
| |    |
| \----/

Intersections occur when two perpendicular paths cross. At an intersection, a
cart is capable of turning left, turning right, or continuing straight. Here
are two loops connected by two intersections:

| /-----\
| |     |
| |  /--+--\
| |  |  |  |
| \--+--/  |
|    |     |
|    \-----/

Several carts are also on the tracks. Carts always face either up (^), down
(v), left (<), or right (>). (On your initial map, the track under each cart is
a straight path matching the direction the cart is facing.)

Each time a cart has the option to turn (by arriving at any intersection), it
turns left the first time, goes straight the second time, turns right the third
time, and then repeats those directions starting again with left the fourth
time, straight the fifth time, and so on. This process is independent of the
particular intersection at which the cart has arrived - that is, the cart has
no per-intersection memory.

Carts all move at the same speed; they take turns moving a single step at a
time. They do this based on their current location: carts on the top row move
first (acting from left to right), then carts on the second row move (again
from left to right), then carts on the third row, and so on. Once each cart has
moved one step, the process repeats; each of these loops is called a tick.

For example, suppose there are two carts on a straight track:

| |  |  |  |  |
| v  |  |  |  |
| |  v  v  |  |
| |  |  |  v  X
| |  |  ^  ^  |
| ^  ^  |  |  |
| |  |  |  |  |

First, the top cart moves. It is facing down (v), so it moves down one square.
Second, the bottom cart moves. It is facing up (^), so it moves up one square.
Because all carts have moved, the first tick ends. Then, the process repeats,
starting with the first cart. The first cart moves down, then the second cart
moves up - right into the first cart, colliding with it! (The location of the
crash is marked with an X.) This ends the second and last tick.

Here is a longer example:

| /->-\        
| |   |  /----\
| | /-+--+-\  |
| | | |  | v  |
| \-+-/  \-+--/
|   \------/   
| 
| /-->\        
| |   |  /----\
| | /-+--+-\  |
| | | |  | |  |
| \-+-/  \->--/
|   \------/   
| 
| /---v        
| |   |  /----\
| | /-+--+-\  |
| | | |  | |  |
| \-+-/  \-+>-/
|   \------/   
| 
| /---\        
| |   v  /----\
| | /-+--+-\  |
| | | |  | |  |
| \-+-/  \-+->/
|   \------/   
| 
| /---\        
| |   |  /----\
| | /->--+-\  |
| | | |  | |  |
| \-+-/  \-+--^
|   \------/   
| 
| /---\        
| |   |  /----\
| | /-+>-+-\  |
| | | |  | |  ^
| \-+-/  \-+--/
|   \------/   
| 
| /---\        
| |   |  /----\
| | /-+->+-\  ^
| | | |  | |  |
| \-+-/  \-+--/
|   \------/   
| 
| /---\        
| |   |  /----<
| | /-+-->-\  |
| | | |  | |  |
| \-+-/  \-+--/
|   \------/   
| 
| /---\        
| |   |  /---<\
| | /-+--+>\  |
| | | |  | |  |
| \-+-/  \-+--/
|   \------/   
| 
| /---\        
| |   |  /--<-\
| | /-+--+-v  |
| | | |  | |  |
| \-+-/  \-+--/
|   \------/   
| 
| /---\        
| |   |  /-<--\
| | /-+--+-\  |
| | | |  | v  |
| \-+-/  \-+--/
|   \------/   
| 
| /---\        
| |   |  /<---\
| | /-+--+-\  |
| | | |  | |  |
| \-+-/  \-<--/
|   \------/   
| 
| /---\        
| |   |  v----\
| | /-+--+-\  |
| | | |  | |  |
| \-+-/  \<+--/
|   \------/   
| 
| /---\        
| |   |  /----\
| | /-+--v-\  |
| | | |  | |  |
| \-+-/  ^-+--/
|   \------/   
| 
| /---\        
| |   |  /----\
| | /-+--+-\  |
| | | |  X |  |
| \-+-/  \-+--/
|   \------/   

After following their respective paths for a while, the carts eventually crash.
To help prevent crashes, you'd like to know the location of the first crash.
Locations are given in X,Y coordinates, where the furthest left column is X=0
and the furthest top row is Y=0:

|            111
|  0123456789012
| 0/---\        
| 1|   |  /----\
| 2| /-+--+-\  |
| 3| | |  X |  |
| 4\-+-/  \-+--/
| 5  \------/   

In this example, the location of the first crash is 7,3.
-}

import Data.List(sort)
import Data.Map(Map,fromList,(!))
import Data.Tuple(swap)

-- Internally, use (y,x) to make sorting carts easier.
parse :: String -> (Map (Int,Int) Char,[((Int,Int),(Char,Int))])
parse s = (fromList tracks,carts)
  where
    (tracks,carts) = p 0 0 [] [] s
    p y x tracks carts "" = (tracks,carts)
    p y x tracks carts ('-':rest) = p y (x+1) (((y,x),'-'):tracks) carts rest
    p y x tracks carts ('|':rest) = p y (x+1) (((y,x),'|'):tracks) carts rest
    p y x tracks carts ('/':rest) = p y (x+1) (((y,x),'/'):tracks) carts rest
    p y x tracks carts ('\\':rest) = p y (x+1) (((y,x),'\\'):tracks) carts rest
    p y x tracks carts ('+':rest) = p y (x+1) (((y,x),'+'):tracks) carts rest
    p y x tracks carts ('v':rest) = p y (x+1) (((y,x),'|'):tracks) (((y,x),('v',0)):carts) rest
    p y x tracks carts ('^':rest) = p y (x+1) (((y,x),'|'):tracks) (((y,x),('^',0)):carts) rest
    p y x tracks carts ('<':rest) = p y (x+1) (((y,x),'-'):tracks) (((y,x),('<',0)):carts) rest
    p y x tracks carts ('>':rest) = p y (x+1) (((y,x),'-'):tracks) (((y,x),('>',0)):carts) rest
    p y x tracks carts ('\n':rest) = p (y+1) 0 tracks carts rest
    p y x tracks carts (_:rest) = p y (x+1) tracks carts rest

tick :: Map (Int,Int) Char -> [((Int,Int),(Char,Int))] -> ([(Int,Int)],[((Int,Int),(Char,Int))])
tick tracks carts = t [] carts []
  where
    t moved [] collisions = (collisions,sort moved)
    t moved (((y,x),(dir,state)):unmoved) collisions
      | (y,x) `elem` collisions = t moved unmoved collisions
      | otherwise = t ((newyx,newdirstate):moved) unmoved newcollisions
      where
        newyx
          | dir == '^' = (y-1,x)
          | dir == '<' = (y,x-1)
          | dir == 'v' = (y+1,x)
          | dir == '>' = (y,x+1)
        newtrack = tracks!newyx
        newdirstate
          | newtrack == '-' = (dir,state)
          | newtrack == '|' = (dir,state)
          | newtrack == '/' && dir == '^' = ('>',state)
          | newtrack == '/' && dir == '>' = ('^',state)
          | newtrack == '/' && dir == 'v' = ('<',state)
          | newtrack == '/' && dir == '<' = ('v',state)
          | newtrack == '\\' && dir == '^' = ('<',state)
          | newtrack == '\\' && dir == '<' = ('^',state)
          | newtrack == '\\' && dir == 'v' = ('>',state)
          | newtrack == '\\' && dir == '>' = ('v',state)
          | newtrack == '+' && state == 0 = (turn dir,1)
          | newtrack == '+' && state == 1 = (dir,2)
          | newtrack == '+' && state == 2 = (turn $ turn $ turn dir,0)
        turn '^' = '<'
        turn '<' = 'v'
        turn 'v' = '>'
        turn '>' = '^'
        newcollisions
          | newyx `elem` (map fst moved ++ map fst unmoved) = (newyx:collisions)
          | otherwise = collisions

-- swap changes Y,X to X,Y for the final result.
run :: String -> (Int,Int)
run s = swap $ last $ fst $ head $ dropWhile (null . fst) $ iterate (tick tracks . snd) ([],carts)
  where (tracks,carts) = parse s

testData :: String
testData = "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   "

test :: ()
test
  | run testData /= (7,3) = error "a"
  | otherwise = ()

part1 :: IO (Int,Int)
part1 = fmap run $ readFile "input/13.txt"

testData2 :: String
testData2 = "/>-<\\  \n|   |  \n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/"

removeCrashed :: ([(Int,Int)],[((Int,Int),(Char,Int))]) -> [((Int,Int),(Char,Int))]
removeCrashed (collisions,carts) = filter (not . (`elem` collisions) . fst) carts

run2 :: String -> (Int,Int)
run2 s = swap $ fst $ head $ head $ dropWhile ((> 1) . length) $ iterate (removeCrashed . tick tracks) carts
  where (tracks,carts) = parse s

test2 :: ()
test2
  | run2 testData2 /= (6,4) = error "a"
  | otherwise = ()

part2 :: IO (Int,Int)
part2 = fmap run2 $ readFile "input/13.txt"
