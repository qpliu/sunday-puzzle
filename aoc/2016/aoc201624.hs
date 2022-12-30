{-
--- Day 24: Air Duct Spelunking ---

You've finally met your match; the doors that provide access to the roof are
locked tight, and all of the controls and related electronics are inaccessible.
You simply can't reach them.

The robot that cleans the air ducts, however, can.

It's not a very fast little robot, but you reconfigure it to be able to
interface with some of the exposed wires that have been routed through the HVAC
system. If you can direct it to each of those locations, you should be able to
bypass the security controls.

You extract the duct layout for this area from some blueprints you acquired and
create a map with the relevant locations marked (your puzzle input). 0 is your
current location, from which the cleaning robot embarks; the other numbers are
(in no particular order) the locations the robot needs to visit at least once
each. Walls are marked as #, and open passages are marked as .. Numbers behave
like open passages.

For example, suppose you have a map like the following:

###########
#0.1.....2#
#.#######.#
#4.......3#
###########

To reach all of the points of interest as quickly as possible, you would have
the robot take the following path:

 - 0 to 4 (2 steps)
 - 4 to 1 (4 steps; it can't move diagonally)
 - 1 to 2 (6 steps)
 - 2 to 3 (2 steps)

Since the robot isn't very fast, you need to find it the shortest route. This
path is the fewest steps (in the above example, a total of 14) required to
start at 0 and then visit every other location at least once.

Given your actual map, and starting from location 0, what is the fewest number
of steps required to visit every non-0 number marked on the map at least once?
-}

-- If the numbers are unique, there are a maximum of 9 locations to visit.
-- It's possible there are more one than location with the same number, though
-- I doubt it.  The numbers are probably used in part 2 so that, say,
-- 4 must be visited after 3 is visited (visiting 4 before visiting 3 would
-- not count.)
--
-- It looks like there are less than 9 locations to visit in my input data.
-- And the numbers are unique.
--
-- Build a graph of the distances between the locations, then brute-force
-- every permutation of the locations.

import Data.Char(isDigit)
import Data.List(permutations)
import Data.Map(Map,keysSet,(!))
import qualified Data.Map
import Data.Set(Set,difference,intersection)
import qualified Data.Set

type Loc = ((Int,Int),Char)

parseMap :: String -> (Loc,Set (Int,Int),Map (Int,Int) Char)
parseMap s = ((start,'0'),Data.Set.fromList openPassages,Data.Map.fromList locations)
  where
    (_,start,openPassages,locations) = foldl scan ((0,0),(-1,-1),[],[]) s
    scan ((x,y),start,opens,locs) c
      | c == '\n' = ((0,y+1),start,opens,locs)
      | c == '.' = ((x+1,y),start,(x,y):opens,locs)
      | c == '0' = ((x+1,y),(x,y),(x,y):opens,locs)
      | isDigit c = ((x+1,y),start,(x,y):opens,((x,y),c):locs)
      | otherwise = ((x+1,y),start,opens,locs)

distancesFrom :: Set (Int,Int) -> Map (Int,Int) Char -> Loc -> [((Loc,Loc),Int)]
distancesFrom openPassages allTargets src@(srcXY,_) = walk 1 (Data.Set.delete srcXY openPassages) (Data.Set.fromList [srcXY]) (Data.Map.delete srcXY allTargets)
  where
    walk nsteps unvisited current targets
      | Data.Map.null targets || Data.Set.null unvisited = []
      | otherwise = map toEdge found ++ walk (nsteps+1) (unvisited `difference` next) next (foldr Data.Map.delete targets found)
      where
        found = Data.Set.toList (keysSet targets `intersection` next)
        next = Data.Set.fromList (concatMap step (Data.Set.toList current)) `intersection` unvisited
        toEdge xy = ((src,(xy,allTargets!xy)),nsteps)
    step (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

makeGraph :: (Loc,Set (Int,Int),Map (Int,Int) Char) -> Map (Loc,Loc) Int
makeGraph (start,openPassages,targets) = Data.Map.fromList $ concatMap (distancesFrom openPassages targets) (start:Data.Map.toList targets)

search :: (Loc,Set (Int,Int),Map (Int,Int) Char) -> Int
search layout@(start,_,targets) = minimum $ map (getSteps 0 start) $ permutations $ Data.Map.toList targets
  where
    graph = makeGraph layout
    getSteps dist loc1 [] = dist
    getSteps dist loc1 (loc2:locs) = getSteps (dist+graph!(loc1,loc2)) loc2 locs

test :: ()
test
  | search (parseMap testData) /= 14 = error "a"
  | otherwise = ()
  where
    testData = "###########\n#0.1.....2#\n#.#######.#\n#4.......3#\n###########"

part1 :: IO Int
part1 = fmap (search . parseMap) $ readFile "input/24.txt"

search2 layout@(start,_,targets) = minimum $ map (getSteps 0 start) $ permutations $ Data.Map.toList (Data.Map.delete (fst start) targets)
  where
    graph = makeGraph layout
    getSteps dist loc1 [] = dist + graph!(start,loc1)
    getSteps dist loc1 (loc2:locs) = getSteps (dist+graph!(loc1,loc2)) loc2 locs

part2 :: IO Int
part2 = fmap (search2 . parseMap) $ readFile "input/24.txt"
