{-
--- Day 22: Sporifica Virus ---

Diagnostics indicate that the local grid computing cluster has been
contaminated with the Sporifica Virus. The grid computing cluster is a
seemingly-infinite two-dimensional grid of compute nodes. Each node is either
clean or infected by the virus.

To prevent overloading the nodes (which would render them useless to the virus)
or detection by system administrators, exactly one virus carrier moves through
the network, infecting or cleaning nodes as it moves. The virus carrier is
always located on a single node in the network (the current node) and keeps
track of the direction it is facing.

To avoid detection, the virus carrier works in bursts; in each burst, it wakes
up, does some work, and goes back to sleep. The following steps are all
executed in order one time each burst:

 - If the current node is infected, it turns to its right. Otherwise, it turns
   to its left. (Turning is done in-place; the current node does not change.)
 - If the current node is clean, it becomes infected. Otherwise, it becomes
   cleaned. (This is done after the node is considered for the purposes of
   changing direction.)
 - The virus carrier moves forward one node in the direction it is facing.

Diagnostics have also provided a map of the node infection status (your puzzle
input). Clean nodes are shown as .; infected nodes are shown as #. This map
only shows the center of the grid; there are many more nodes beyond those
shown, but none of them are currently infected.

The virus carrier begins in the middle of the map facing up.

For example, suppose you are given a map like this:

| ..#
| #..
| ...

Then, the middle of the infinite grid looks like this, with the virus carrier's
position marked with [ ]:

| . . . . . . . . .
| . . . . . . . . .
| . . . . . . . . .
| . . . . . # . . .
| . . . #[.]. . . .
| . . . . . . . . .
| . . . . . . . . .
| . . . . . . . . .

The virus carrier is on a clean node, so it turns left, infects the node, and
moves left:

| . . . . . . . . .
| . . . . . . . . .
| . . . . . . . . .
| . . . . . # . . .
| . . .[#]# . . . .
| . . . . . . . . .
| . . . . . . . . .
| . . . . . . . . .

The virus carrier is on an infected node, so it turns right, cleans the node,
and moves up:

| . . . . . . . . .
| . . . . . . . . .
| . . . . . . . . .
| . . .[.]. # . . .
| . . . . # . . . .
| . . . . . . . . .
| . . . . . . . . .
| . . . . . . . . .

Four times in a row, the virus carrier finds a clean, infects it, turns left,
and moves forward, ending in the same place and still facing up:

| . . . . . . . . .
| . . . . . . . . .
| . . . . . . . . .
| . . #[#]. # . . .
| . . # # # . . . .
| . . . . . . . . .
| . . . . . . . . .
| . . . . . . . . .

Now on the same node as before, it sees an infection, which causes it to turn
right, clean the node, and move forward:

| . . . . . . . . .
| . . . . . . . . .
| . . . . . . . . .
| . . # .[.]# . . .
| . . # # # . . . .
| . . . . . . . . .
| . . . . . . . . .
| . . . . . . . . .

After the above actions, a total of 7 bursts of activity had taken place. Of
them, 5 bursts of activity caused an infection.

After a total of 70, the grid looks like this, with the virus carrier facing
up:

| . . . . . # # . .
| . . . . # . . # .
| . . . # . . . . #
| . . # . #[.]. . #
| . . # . # . . # .
| . . . . . # # . .
| . . . . . . . . .
| . . . . . . . . .

By this time, 41 bursts of activity caused an infection (though most of those
nodes have since been cleaned).

After a total of 10000 bursts of activity, 5587 bursts will have caused an
infection.

Given your actual map, after 10000 bursts of activity, how many bursts cause a
node to become infected? (Do not count nodes that begin infected.)
-}

import Data.Map(Map,findWithDefault)
import qualified Data.Map
import Data.Set(Set,delete,fromList,insert,member,toList)

parse :: String -> (Int,((Int,Int),(Int,Int),Set (Int,Int)))
parse s = p s 0 0 0 0 []
  where
    p ('.':chars) x y xmax ymax points = p chars (x+1) y (max x xmax) (max y ymax) points
    p ('#':chars) x y xmax ymax points = p chars (x+1) y (max x xmax) (max y ymax) ((x,y):points)
    p ('\n':chars) x y xmax ymax points = p chars 0 (y+1) xmax ymax points
    p (_:chars) x y xmax ymax points = p chars x y xmax ymax points
    p "" _ _ xmax ymax points = (0,((xmax `div` 2,ymax `div` 2),(0,-1),fromList points))

burst :: (Int,((Int,Int),(Int,Int),Set (Int,Int))) -> (Int,((Int,Int),(Int,Int),Set (Int,Int)))
burst (infectCount,((x,y),(dx,dy),grid))
  | (x,y) `member` grid = (infectCount,((x-dy,y+dx),(-dy,dx),delete (x,y) grid))
  | otherwise = (infectCount+1,((x+dy,y-dx),(dy,-dx),insert (x,y) grid))

testData :: String
testData = "..#\n#..\n..."

test :: ()
test
  | fst (head $ drop 7 $ iterate burst $ parse testData) /= 5 = error "a"
  | fst (head $ drop 70 $ iterate burst $ parse testData) /= 41 = error "b"
  | fst (head $ drop 10000 $ iterate burst $ parse testData) /= 5587 = error "c"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (fst . head . drop 10000 . iterate burst . parse) $ readFile "input/22.txt"

data State = Clean | Weakened | Infected | Flagged deriving Eq

parse2 :: String -> (Int,((Int,Int),(Int,Int),Map (Int,Int) State))
parse2 s = (infectCount,((x,y),(dx,dy),convert grid))
  where
    (infectCount,((x,y),(dx,dy),grid)) = parse s
    convert grid = Data.Map.fromList [((x,y),Infected) | (x,y) <- toList grid]

burst2 :: (Int,((Int,Int),(Int,Int),Map (Int,Int) State)) -> (Int,((Int,Int),(Int,Int),Map (Int,Int) State))
burst2 (infectCount,((x,y),(dx,dy),grid)) = b (findWithDefault Clean (x,y) grid)
  where
    b Clean = (infectCount,((x+dy,y-dx),(dy,-dx),Data.Map.insert (x,y) Weakened grid))
    b Weakened = (infectCount+1,((x+dx,y+dy),(dx,dy),Data.Map.insert (x,y) Infected grid))
    b Infected = (infectCount,((x-dy,y+dx),(-dy,dx),Data.Map.insert (x,y) Flagged grid))
    b Flagged = (infectCount,((x-dx,y-dy),(-dx,-dy),Data.Map.delete (x,y) grid))

test2 :: ()
test2
  | fst (head $ drop 100 $ iterate burst2 $ parse2 testData) /= 26 = error "a"
  | fst (head $ drop 10000000 $ iterate burst2 $ parse2 testData) /= 2511944 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (fst . head . drop 10000000 . iterate burst2 . parse2) $ readFile "input/22.txt"
