import Data.Map(Map,keys,mapWithKey,(!))
import qualified Data.Map
import Data.Set(Set,difference,intersection,union,unions)
import qualified Data.Set

primes :: [Int]
primes = 2:3:s (tail primes) [5,7..]
  where s (p:ps) xs = h ++ s ps [x | x <- t, x `mod` p /= 0]
            where (h,(_:t)) = span (< p*p) xs

oneStep :: Int -> Int -> Bool
oneStep x y
  | x == 0 || y == 0 = False
  | x `mod` 10 == y `mod` 10 = oneStep (x `div` 10) (y `div` 10)
  | otherwise = x `div` 10 == y `div` 10

makeGraph :: [Int] -> Map Int (Set Int)
makeGraph ns = Data.Map.fromList [(n,Data.Set.fromList (filter (oneStep n) ns)) | n <- ns]

initialLadder :: Map Int (Set Int) -> Map Int (Int,(Set Int,Set Int,[Set Int]))
initialLadder graph = mapWithKey init graph
  where
    init i oneAway = (0,(iset,iset,[iset])) where iset = Data.Set.fromList [i]

addStep :: Map Int (Set Int) -> Map Int (Int,(Set Int,Set Int,[Set Int])) -> Map Int (Int,(Set Int,Set Int,[Set Int]))
addStep graph ladders = Data.Map.map add ladders
  where
    add :: (Int,(Set Int,Set Int,[Set Int])) ->  (Int,(Set Int,Set Int,[Set Int]))
    add (n,(nAway,closer,chain))
      | Data.Set.null nAway = (n,(nAway,closer,chain))
      | otherwise = (n+1,(nPlus1Away,union nPlus1Away closer,nPlus1Away:chain))
      where
        nPlus1Away = difference (unions (map (graph!) (Data.Set.toList nAway))) closer

findAMax :: Map Int (Set Int) -> (Int,(Set Int,Set Int,[Set Int]))
findAMax graph = search (-1) (initialLadder graph)
  where
    search maxsteps ladder
      | maxsteps == nsteps = maximum ladder
      | otherwise = search nsteps (addStep graph (Data.Map.filter ((== nsteps) . fst) ladder))
      where nsteps = fst (maximum ladder)

formatHit :: (Int,(Set Int,Set Int,[Set Int])) -> (Int,[Int])
formatHit (n,(_,_,_:l)) = (n,formatList l (const True))
  where
    formatList [] _ = []
    formatList (i:l) f = let j = head (filter f (Data.Set.toList i)) in j : formatList l (oneStep j)

connectedSubgraph :: Map Int (Set Int) -> Int -> Set Int
connectedSubgraph graph n = collect [n] Data.Set.empty
  where
    collect [] set = set
    collect (n:ns) set
      | Data.Set.member n set = collect ns set
      | otherwise = collect (ns++Data.Set.toList (graph!n)) (Data.Set.insert n set)

findLadder :: Map Int (Set Int) -> Int -> Int -> [Int]
findLadder graph a b = search (initAset,initAset,[]) (initBset,initBset,[])
  where
    initAset = Data.Set.fromList [a]
    initBset = Data.Set.fromList [b]
    search (atops,aset,alist) (btops,bset,blist)
      | not (Data.Set.null abMeets) = reverse (tail (thread abMeet alist)) ++ abMeet:tail (thread abMeet blist)
      | otherwise = search (btops,bset,blist) (newAtops,union newAtops aset,atops:alist)
      where
        newAtops = difference (unions (map (graph!) (Data.Set.toList atops))) aset
        abMeets = intersection atops btops
        abMeet = head (Data.Set.toList abMeets)
        thread n [] = [n]
        thread n (set:sets) = n : thread (head (filter (oneStep n) (Data.Set.toList set))) sets

findMaxLadder :: Map Int (Set Int) -> Int -> (Int,[Int])
findMaxLadder graph maxConnections =
    maximum [let ladder = findLadder graph a b in (length ladder,ladder) | a <- starts, b <- starts, b > a && not (oneStep a b)]
  where
    starts = keys (Data.Map.filter (\ connections -> let n = Data.Set.size connections in n > 0 && n <= maxConnections) graph)

findLongestLadderFrom :: Map Int (Set Int) -> Int -> [Int]
findLongestLadderFrom graph i =  chooseOne (addConnections iset iset [])
  where
    iset = Data.Set.fromList [i]
    chooseOne [] = []
    chooseOne [set] = [minimum set]
    chooseOne (set:set1:sets) = let n = (minimum set) in n : chooseOne (Data.Set.filter (oneStep n) set1:sets)
    addConnections top all ladders
      | Data.Set.null top = ladders
      | otherwise = addConnections newTop (union newTop all) (top:ladders)
      where
        newTop = difference (unions (map (graph!) (Data.Set.toList top))) all

main :: IO ()
main = do
    p 10
    p 100
    p 1000
    p 10000
    p2 10 5
    p2 100 5
    p2 1000 5
    p2 10000 4
    p2 100000 1
  where
    p n = print $ formatHit $ findAMax $ makeGraph $ takeWhile (< 10*n) $ dropWhile (< n) primes
    p2 n m = print $ findMaxLadder (makeGraph $ takeWhile (< 10*n) $ dropWhile (< n) primes) m
