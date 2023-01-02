{-
--- Day 12: Digital Plumber ---

Walking along the memory banks of the stream, you find a small village that is
experiencing a little confusion: some programs can't communicate with each
other.

Programs in this village communicate using a fixed system of pipes. Messages
are passed between programs using these pipes, but most programs aren't
connected to each other directly. Instead, programs pass messages between each
other until the message reaches the intended recipient.

For some reason, though, some of these messages aren't ever reaching their
intended recipient, and the programs suspect that some pipes are missing. They
would like you to investigate.

You walk through the village and record the ID of each program and the IDs with
which it can communicate directly (your puzzle input). Each program has one or
more programs with which it can communicate, and these pipes are bidirectional;
if 8 says it can communicate with 11, then 11 will say it can communicate with
8.

You need to figure out how many programs are in the group that contains program
ID 0.

For example, suppose you go door-to-door like a travelling salesman and record
the following list:

| 0 <-> 2
| 1 <-> 1
| 2 <-> 0, 3, 4
| 3 <-> 2, 4
| 4 <-> 2, 3, 6
| 5 <-> 6
| 6 <-> 4, 5

In this example, the following programs are in the group that contains program
ID 0:

 - Program 0 by definition.
 - Program 2, directly connected to program 0.
 - Program 3 via program 2.
 - Program 4 via program 2.
 - Program 5 via programs 6, then 4, then 2.
 - Program 6 via programs 4, then 2.

Therefore, a total of 6 programs are in this group; all but program 1, which
has a pipe that connects it to itself.

How many programs are in the group that contains program ID 0?
-}

import Data.Map(Map,filterWithKey,keysSet,(!))
import qualified Data.Map
import Data.Set(Set,difference,size,intersection,member,union,unions)
import qualified Data.Set

parse :: String -> Map String (Set String)
parse s = Data.Map.fromList $ map (parse1 . words) $ lines s
  where
    parse1 (p:"<->":ps) = (p,Data.Set.fromList (map (filter (/= ',')) ps))

connectedTo :: String -> Map String (Set String) -> Set String
connectedTo program graph = walk (Data.Set.fromList [program]) (Data.Set.fromList [program])
  where
    walk seen current
      | size current == 0 = seen
      | otherwise = walk (seen `union` newCurrent) newCurrent
      where
        newCurrent = (unions $ map (graph!) $ Data.Set.toList current) `difference` seen

testData :: String
testData = "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5"

test :: ()
test
  | size (connectedTo "0" $ parse testData) /= 6 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (size . connectedTo "0" . parse) $ readFile "input/12.txt"

groups :: Map String (Set String) -> [Set String]
groups graph
  | Data.Map.null graph = []
  | otherwise = group : groups (filterWithKey notInGroup graph)
  where
    group = connectedTo (minimum $ keysSet graph) graph
    notInGroup prog _ = not (prog `member` group)

test2 :: ()
test2
  | length (groups $ parse testData) /= 2 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (length . groups . parse) $ readFile "input/12.txt"
