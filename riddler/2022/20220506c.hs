import Data.Foldable(maximumBy)
import Data.Map(Map,mapWithKey,(!))
import qualified Data.Map
import Data.Set(Set,difference,union,unions)
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

main :: IO ()
main = do
    p 10
    p 100
    p 1000
    p 10000
    p 100000
    p 1000000
  where
    p n = print $ formatHit $ findAMax $ makeGraph $ takeWhile (< 10*n) $ dropWhile (< n) primes
