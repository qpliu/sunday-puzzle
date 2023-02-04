import Data.List(permutations)
import Data.Map(Map,(!))
import qualified Data.Map
import Data.Set(Set)
import qualified Data.Set

collect :: (Set String,Map (String,String) Int) -> String -> (Set String,Map (String,String) Int)
collect (s,m) distance = parse (words distance)
  where
    parse (n1:"to":n2:"=":dist:_) = (Data.Set.insert n1 (Data.Set.insert n2 s),Data.Map.insert (min n1 n2,max n1 n2) (read dist) m)

parse :: String -> (Set String,Map (String,String) Int)
parse s = foldl collect (Data.Set.empty,Data.Map.empty) (lines s)

totalDistance :: Map (String,String) Int -> [String] -> Int
totalDistance dists route = dist route
  where
    dist (a:as@(b:_)) = dists!(min a b,max a b) + dist as
    dist _ = 0

-- Brute force is probably too slow for the actual input.
bruteforce :: (Set String,Map (String,String) Int) -> Int
bruteforce (s,m) = minimum $ map (totalDistance m) (filter (\ l -> head l > last l) (permutations (Data.Set.toList s)))

-- Nearest neighbor doesn't necessarily find the shortest route.
nearestneighbor :: (Set String,Map (String,String) Int) -> (Int,[String])
nearestneighbor (allset,dists) = addnearests (startdist,starta,[],startb,[],startset)
  where
    (startdist,(starta,startb)) = minimum $ map (\ (k,v) -> (v,k)) $ Data.Map.toList dists
    startset = Data.Set.delete starta $ Data.Set.delete startb allset
    addnearests (dist,a,patha,b,pathb,unvisited)
      | Data.Set.null unvisited = (dist,(a:patha) ++ reverse (b:pathb))
      | otherwise = addnearests $ minimum $ map addnext $ Data.Set.toList unvisited
      where
        addnext c
          | dists!(min a c,max a c) < dists!(min b c,max b c) = (dist+dists!(min a c,max a c),c,a:patha,b,pathb,Data.Set.delete c unvisited)
          | otherwise = (dist+dists!(min b c,max b c),a,patha,c,b:pathb,Data.Set.delete c unvisited)

test :: ()
test
  | bruteforce dists /= 605 = error "a"
  | fst (nearestneighbor dists) /= 605 = error "b"
  | otherwise = ()
  where
    dists = parse "London to Dublin = 464\nLondon to Belfast = 518\nDublin to Belfast = 141"

-- The input is small enough to use brute-force.

part1 :: IO Int
part1 = fmap (bruteforce . parse) (readFile "input/09.txt")

part2bruteforce :: (Set String,Map (String,String) Int) -> Int
part2bruteforce (s,m) = maximum $ map (totalDistance m) (filter (\ l -> head l > last l) (permutations (Data.Set.toList s)))

part2 :: IO Int
part2 = fmap (part2bruteforce . parse) (readFile "input/09.txt")
