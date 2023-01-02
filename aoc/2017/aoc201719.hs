{-
--- Day 19: A Series of Tubes ---

Somehow, a network packet got lost and ended up here. It's trying to follow a
routing diagram (your puzzle input), but it's confused about where to go.

Its starting point is just off the top of the diagram. Lines (drawn with |, -,
and +) show the path it needs to take, starting by going down onto the only
line connected to the top of the diagram. It needs to follow this path until it
reaches the end (located somewhere within the diagram) and stop there.

Sometimes, the lines cross over each other; in these cases, it needs to
continue going the same direction, and only turn left or right when there's no
other option. In addition, someone has left letters on the line; these also
don't change its direction, but it can use them to keep track of where it's
been. For example:

|      |          
|      |  +--+    
|      A  |  C    
|  F---|----E|--+ 
|      |  |  |  D 
|      +B-+  +--+ 

Given this diagram, the packet needs to take the following path:

 - Starting at the only line touching the top of the diagram, it must go down,
   pass through A, and continue onward to the first +.
 - Travel right, up, and right, passing through B in the process.
 - Continue down (collecting C), right, and up (collecting D).
 - Finally, go all the way left through E and stopping at F.

Following the path to the end, the letters it sees on its path are ABCDEF.

The little packet looks up at you, hoping you can help it find the way. What letters will it see (in the order it would see them) if it follows the path? (The routing diagram is very wide; make sure you view it without line wrapping.)
-}

import Data.Map(Map,fromList,findWithDefault)

parse :: String -> ((Int,Int),Map (Int,Int) Char)
parse s = (fst $ minimum $ filter ((/= ' ') . snd) list,fromList list)
  where
    list = concat $ zipWith parseRow [0..] (lines s)
    parseRow row line = zip (map ((,) row) [0..]) line

follow :: Map (Int,Int) Char -> (Int,Int) -> (Int,Int) -> String
follow diagram (y,x) (dy,dx)
  | (dy,dx) == (0,0) = ""
  | c (y,x) `elem` "|-+" = follow diagram (newy,newx) (newdy,newdx)
  | otherwise = c (y,x) : follow diagram (newy,newx) (newdy,newdx)
  where
    c pos = findWithDefault ' ' pos diagram
    (newy,newx) = (y+newdy,x+newdx)
    (newdy,newdx)
      | c (y+dy,x+dx) /= ' ' = (dy,dx)
      | c (y+dx,x+dy) /= ' ' = (dx,dy)
      | c (y-dx,x-dy) /= ' ' = (-dx,-dy)
      | otherwise = (0,0)

letters :: String -> String
letters s = follow diagram start (1,0)
  where (start,diagram) = parse s

testData :: String
testData = "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ "

test :: ()
test
  | letters testData /= "ABCDEF" = error "a"
  | otherwise = ()

part1 :: IO String
part1 = fmap letters $ readFile "input/19.txt"

follow2 :: Map (Int,Int) Char -> (Int,Int) -> (Int,Int) -> Int -> Int
follow2 diagram (y,x) (dy,dx) count
  | (dy,dx) == (0,0) = count
  | otherwise = follow2 diagram (newy,newx) (newdy,newdx) (count+1)
  where
    c pos = findWithDefault ' ' pos diagram
    (newy,newx) = (y+newdy,x+newdx)
    (newdy,newdx)
      | c (y+dy,x+dx) /= ' ' = (dy,dx)
      | c (y+dx,x+dy) /= ' ' = (dx,dy)
      | c (y-dx,x-dy) /= ' ' = (-dx,-dy)
      | otherwise = (0,0)

countSteps :: String -> Int
countSteps s = follow2 diagram start (1,0) 0
  where (start,diagram) = parse s

test2 :: ()
test2
  | countSteps testData /= 38 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap countSteps $ readFile "input/19.txt"
