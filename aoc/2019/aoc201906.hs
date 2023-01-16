{-
--- Day 6: Universal Orbit Map ---

You've landed at the Universal Orbit Map facility on Mercury. Because
navigation in space often involves transferring between orbits, the orbit maps
here are useful for finding efficient routes between, for example, you and
Santa. You download a map of the local orbits (your puzzle input).

Except for the universal Center of Mass (COM), every object in space is in
orbit around exactly one other object. An orbit looks roughly like this:

|                   \
|                    \
|                     |
|                     |
| AAA--> o            o <--BBB
|                     |
|                     |
|                    /
|                   /

In this diagram, the object BBB is in orbit around AAA. The path that BBB takes
around AAA (drawn with lines) is only partly shown. In the map data, this
orbital relationship is written AAA)BBB, which means "BBB is in orbit around
AAA".

Before you use your map data to plot a course, you need to make sure it wasn't
corrupted during the download. To verify maps, the Universal Orbit Map facility
uses orbit count checksums - the total number of direct orbits (like the one
shown above) and indirect orbits.

Whenever A orbits B and B orbits C, then A indirectly orbits C. This chain can
be any number of objects long: if A orbits B, B orbits C, and C orbits D, then
A indirectly orbits D.

For example, suppose you have the following map:

| COM)B
| B)C
| C)D
| D)E
| E)F
| B)G
| G)H
| D)I
| E)J
| J)K
| K)L

Visually, the above map of orbits looks like this:

|         G - H       J - K - L
|        /           /
| COM - B - C - D - E - F
|                \
|                 I

In this visual representation, when two objects are connected by a line, the
one on the right directly orbits the one on the left.

Here, we can count the total number of orbits as follows:

 - D directly orbits C and indirectly orbits B and COM, a total of 3 orbits.
 - L directly orbits K and indirectly orbits J, E, D, C, B, and COM, a total of
   7 orbits.
 - COM orbits nothing.
The total number of direct and indirect orbits in this example is 42.

What is the total number of direct and indirect orbits in your map data?
-}

import Data.Map(Map,alter,empty,fromList,member,size,toList,unionWith,(!))
import qualified Data.Map
import Data.Set(difference,union)
import qualified Data.Set

parse :: String -> Map String String
parse = fromList . map p . words
  where p str = (bbb,aaa) where (aaa,(')':bbb)) = span (/= ')') str

testData :: String
testData = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"

toIndirect :: Map String String -> Map String Int
toIndirect direct = indirect
  where
    indirect = fromList $ map makeCount $ toList direct
    makeCount (bbb,aaa)
      | not (member aaa direct) = (bbb,0)
      | otherwise = (bbb,1 + indirect!aaa)

toCounts :: String -> (Int,Int)
toCounts input = (size direct,sum indirect)
  where
    direct = parse input
    indirect = toIndirect direct

test :: ()
test
  | uncurry (+) (toCounts testData) /= 42 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (uncurry (+) . toCounts) $ readFile "input/06.txt"

testData2 :: String
testData2 = "OM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"

invert :: Map String String -> Map String [String]
invert m = foldr collect empty $ toList m
  where
    collect (bbb,aaa) inv = alter (Just . maybe [bbb] (bbb:)) aaa inv

toGraph :: Map String String -> (String,String,Map String [String])
toGraph direct = (direct!"YOU", direct!"SAN", unionWith (++) (Data.Map.map (:[]) direct) (invert direct))

search :: (String,String,Map String [String]) -> Int
search (start,end,graph) = s 0 (Data.Set.fromList [start]) (Data.Set.fromList [start])
  where
    s n seen current
      | Data.Set.member end current = n
      | otherwise = s (n+1) (union seen next) (difference next seen)
      where
        next = Data.Set.fromList $ concatMap (graph!) $ Data.Set.toList current

test2 :: ()
test2
  | (search $ toGraph $ parse testData2) /= 4 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (search . toGraph . parse) $ readFile "input/06.txt"
