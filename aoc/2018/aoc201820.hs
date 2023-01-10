{-
--- Day 20: A Regular Map ---

While you were learning about instruction pointers, the Elves made considerable
progress. When you look up, you discover that the North Pole base construction 
roject has completely surrounded you.

The area you are in is made up entirely of rooms and doors. The rooms are
arranged in a grid, and rooms only connect to adjacent rooms when a door is
present between them.

For example, drawing rooms as ., walls as #, doors as | or -, your current
position as X, and where north is up, the area you're in might look like this:

| #####
| #.|.#
| #-###
| #.|X#
| #####

You get the attention of a passing construction Elf and ask for a map. "I don't
have time to draw out a map of this place - it's huge. Instead, I can give you
directions to every room in the facility!" He writes down some directions on a
piece of parchment and runs off. In the example above, the instructions might
have been ^WNE$, a regular expression or "regex" (your puzzle input).

The regex matches routes (like WNE for "west, north, east") that will take you
from your current room through various doors in the facility. In aggregate, the
routes will take you through every door in the facility at least once; mapping
out all of these routes will let you build a proper map and find your way
around.

^ and $ are at the beginning and end of your regex; these just mean that the
regex doesn't match anything outside the routes it describes. (Specifically, ^
matches the start of the route, and $ matches the end of it.) These characters
will not appear elsewhere in the regex.

The rest of the regex matches various sequences of the characters N (north), S
(south), E (east), and W (west). In the example above, ^WNE$ matches only one
route, WNE, which means you can move west, then north, then east from your
current position. Sequences of letters like this always match that exact route
in the same order.

Sometimes, the route can branch. A branch is given by a list of options
separated by pipes (|) and wrapped in parentheses. So, ^N(E|W)N$ contains a
branch: after going north, you must choose to go either east or west before
finishing your route by going north again. By tracing out the possible routes
after branching, you can determine where the doors are and, therefore, where
the rooms are in the facility.

For example, consider this regex: ^ENWWW(NEEE|SSE(EE|N))$

This regex begins with ENWWW, which means that from your current position, all
routes must begin by moving east, north, and then west three times, in that
order. After this, there is a branch. Before you consider the branch, this is
what you know about the map so far, with doors you aren't sure about marked
with a ?:

| #?#?#?#?#
| ?.|.|.|.?
| #?#?#?#-#
|     ?X|.?
|     #?#?#

After this point, there is (NEEE|SSE(EE|N)). This gives you exactly two
options: NEEE and SSE(EE|N). By following NEEE, the map now looks like this:

| #?#?#?#?#
| ?.|.|.|.?
| #-#?#?#?#
| ?.|.|.|.?
| #?#?#?#-#
|     ?X|.?
|     #?#?#

Now, only SSE(EE|N) remains. Because it is in the same parenthesized group as
NEEE, it starts from the same room NEEE started in. It states that starting
from that point, there exist doors which will allow you to move south twice,
then east; this ends up at another branch. After that, you can either move east
twice or north once. This information fills in the rest of the doors:

| #?#?#?#?#
| ?.|.|.|.?
| #-#?#?#?#
| ?.|.|.|.?
| #-#?#?#-#
| ?.?.?X|.?
| #-#-#?#?#
| ?.|.|.|.?
| #?#?#?#?#

Once you've followed all possible routes, you know the remaining unknown parts
are all walls, producing a finished map of the facility:

| #########
| #.|.|.|.#
| #-#######
| #.|.|.|.#
| #-#####-#
| #.#.#X|.#
| #-#-#####
| #.|.|.|.#
| #########

Sometimes, a list of options can have an empty option, like (NEWS|WNSE|). This
means that routes at this point could effectively skip the options in
parentheses and move on immediately. For example, consider this regex and the
corresponding map:

| ^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$
| 
| ###########
| #.|.#.|.#.#
| #-###-#-#-#
| #.|.|.#.#.#
| #-#####-#-#
| #.#.#X|.#.#
| #-#-#####-#
| #.#.|.|.|.#
| #-###-###-#
| #.|.|.#.|.#
| ###########

This regex has one main route which, at three locations, can optionally include
additional detours and be valid: (NEWS|), (WNSE|), and (SWEN|). Regardless of
which option is taken, the route continues from the position it is left at
after taking those steps. So, for example, this regex matches all of the
following routes (and more that aren't listed here):

 - ENNWSWWSSSEENEENNN
 - ENNWSWWNEWSSSSEENEENNN
 - ENNWSWWNEWSSSSEENEESWENNNN
 - ENNWSWWSSSEENWNSEEENNN

By following the various routes the regex matches, a full map of all of the
doors and rooms in the facility can be assembled.

To get a sense for the size of this facility, you'd like to determine which
room is furthest from you: specifically, you would like to find the room for
which the shortest path to that room would require passing through the most
doors.

 - In the first example (^WNE$), this would be the north-east corner 3 doors
   away.
 - In the second example (^ENWWW(NEEE|SSE(EE|N))$), this would be the
   south-east corner 10 doors away.
 - In the third example (^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$), this would
   be the north-east corner 18 doors away.

Here are a few more examples:

| Regex: ^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$
| Furthest room requires passing 23 doors
| 
| #############
| #.|.|.|.|.|.#
| #-#####-###-#
| #.#.|.#.#.#.#
| #-#-###-#-#-#
| #.#.#.|.#.|.#
| #-#-#-#####-#
| #.#.#.#X|.#.#
| #-#-#-###-#-#
| #.|.#.|.#.#.#
| ###-#-###-#-#
| #.|.#.|.|.#.#
| #############

| Regex: ^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$
| Furthest room requires passing 31 doors
| 
| ###############
| #.|.|.|.#.|.|.#
| #-###-###-#-#-#
| #.|.#.|.|.#.#.#
| #-#########-#-#
| #.#.|.|.|.|.#.#
| #-#-#########-#
| #.#.#.|X#.|.#.#
| ###-#-###-#-#-#
| #.|.#.#.|.#.|.#
| #-###-#####-###
| #.|.#.|.|.#.#.#
| #-#-#####-#-#-#
| #.#.|.|.|.#.|.#
| ###############

What is the largest number of doors you would be required to pass through to
reach a room? That is, find the room for which the shortest path from your
starting location to that room would require passing through the most doors;
what is the fewest doors you can pass through to reach it?
-}

import Data.Map(Map,alter,empty,(!))
import qualified Data.Map
import Data.Set(Set,difference,fromList,insert,size,toList,union)
import qualified Data.Set

data Regex = Path String | Choice [[Regex]] deriving Show

parse :: String -> [Regex]
parse = fst . p [] . takeWhile (/= '$') . drop 1 . dropWhile (/= '^')
  where
    p revRXs "" = (reverse revRXs,"")
    p revRXs ('(':rest) = p (choice:revRXs) afterChoice
      where (choice,afterChoice) = parseChoice [] rest
    p revRXs rest@('|':_) = (reverse revRXs,rest)
    p revRXs rest@(')':_) = (reverse revRXs,rest)
    p revRXs rest = p (Path path:revRXs) afterPath
      where (path,afterPath) = span (not . (`elem` "(|)")) rest
    parseChoice revChoices rest
      | take 1 afterChoice == "|" = parseChoice (choice:revChoices) (drop 1 afterChoice)
      | otherwise = (Choice $ reverse (choice:revChoices),drop 1 afterChoice)
      where
        (choiceOrEmpty,afterChoice) = p [] rest
        choice | null choiceOrEmpty = [Path ""] | otherwise = choiceOrEmpty

rev :: Char -> Char
rev 'N' = 'S'
rev 'S' = 'N'
rev 'E' = 'W'
rev 'W' = 'E'

move :: (Int,Int) -> Char -> (Int,Int)
move (x,y) 'N' = (x,y-1)
move (x,y) 'S' = (x,y+1)
move (x,y) 'E' = (x+1,y)
move (x,y) 'W' = (x-1,y)

mapout :: (Set (Int,Int),Map (Int,Int) (Set Char)) -> [Regex] -> (Set (Int,Int),Map (Int,Int) (Set Char))
mapout (xys,m) rxs = foldl mapout1 (xys,m) rxs

mapout1 :: (Set (Int,Int),Map (Int,Int) (Set Char)) -> Regex -> (Set (Int,Int),Map (Int,Int) (Set Char))
mapout1 (startXYs,startM) (Path path) = (fromList endXYs,endM)
  where (endXYs,endM) = foldr (mapPath path) ([],startM) (toList startXYs)
mapout1 (startXYs,startM) (Choice choices) = foldr mergeChoice (Data.Set.empty,startM) choices
  where
    mergeChoice choice (endXYs,m) = (union endXYs xys,m1)
      where (xys,m1) = mapout (startXYs,m) choice

mapPath :: String -> (Int,Int) -> ([(Int,Int)],Map (Int,Int) (Set Char)) -> ([(Int,Int)],Map (Int,Int) (Set Char))
mapPath path startXY (endXYs,startM) = (endXY:endXYs,endM)
  where
    (endXY,endM) = foldl mapPath1 (startXY,startM) path
    mapPath1 (xy,m) dir = (xy1,m2)
      where
        xy1 = move xy dir
        m1 = alter (Just . maybe (fromList [dir]) (insert dir)) xy m
        m2 = alter (Just . maybe (fromList [rev dir]) (insert (rev dir))) xy1 m1

furthestDistance :: (Int,Int) -> Map (Int,Int) (Set Char) -> Int
furthestDistance start m = search 0 (fromList [start]) (fromList [start])
  where
    search nsteps seen current
      | size current == 0 = nsteps-1
      | otherwise = search (nsteps+1) (seen `union` next) (next `difference` seen)
      where
        next = fromList (concatMap step (toList current))
        step xy = map (move xy) (toList (m!xy))

run :: String -> Int
run = furthestDistance (0,0) . snd . mapout (fromList [(0,0)],empty) . parse

test :: ()
test
  | run "^WNE$" /= 3 = error "a"
  | run "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$" /= 18 = error "b"
  | run "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$" /= 23 = error "c"
  | run "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$" /= 31 = error "d"
  | otherwise = ()

part1 :: IO Int
part1 = fmap run $ readFile "input/20.txt"

furtherThan :: Int -> (Int,Int) -> Map (Int,Int) (Set Char) -> Int
furtherThan maxDist start m = search 0 (fromList [start]) (fromList [start])
  where
    search nsteps seen current
      | size current == 0 || nsteps+1 >= maxDist = Data.Map.size m-size seen
      | otherwise = search (nsteps+1) (seen `union` next) (next `difference` seen)
      where
        next = fromList (concatMap step (toList current))
        step xy = map (move xy) (toList (m!xy))

run2 :: Int -> String -> Int
run2 maxDist = furtherThan maxDist (0,0) . snd . mapout (fromList [(0,0)],empty) . parse

part2 :: IO Int
part2 = fmap (run2 1000) $ readFile "input/20.txt"
