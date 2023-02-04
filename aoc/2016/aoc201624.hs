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
