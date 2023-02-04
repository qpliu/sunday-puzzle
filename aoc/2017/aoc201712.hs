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
