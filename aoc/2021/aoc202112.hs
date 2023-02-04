import Data.Char(isLower)
import Data.Map(Map,alter,(!))
import qualified Data.Map
import Data.Set(Set,empty,insert,member)

parse :: String -> Map String [String]
parse = foldr addEdge Data.Map.empty . words
  where
    addEdge edge graph = alter (Just . maybe [a] (a:)) b
                        (alter (Just . maybe [b] (b:)) a graph)
      where
        (a,dashb) = span (/= '-') edge
        b = drop 1 dashb

paths :: String -> Map String [String] -> Set String -> String -> [[String]]
paths dest graph forbidden loc
  | loc == dest = [[loc]]
  | member loc forbidden = []
  | otherwise = concatMap (map (loc:) . paths dest graph nextForbidden) (graph!loc)
  where
    nextForbidden
      | any isLower loc = insert loc forbidden
      | otherwise = forbidden

pathCount :: String -> String -> Map String [String] -> Int
pathCount start end graph = length $ paths end graph empty start

testData :: [(Int,String)]
testData = [
    (10,"start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end\n"),
    (19,"dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc\n"),
    (226,"fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW\n")
    ]

test :: ()
test
  | any (uncurry (/=) . fmap (pathCount "start" "end" . parse)) testData = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (pathCount "start" "end" . parse) $ readFile "input/12.txt"

paths2 :: String -> Map String [String] -> Set String -> Bool -> String -> [[String]]
paths2 dest graph forbidden doubleVisited loc
  | loc == dest = [[loc]]
  | member loc forbidden && doubleVisited = []
  | member loc forbidden = concatMap (map (loc:) . paths2 dest graph nextForbidden True) (filter (/= "start") (graph!loc))
  | otherwise = concatMap (map (loc:) . paths2 dest graph nextForbidden doubleVisited) (filter (/= "start") (graph!loc))
  where
    nextForbidden
      | any isLower loc = insert loc forbidden
      | otherwise = forbidden

pathCount2 :: String -> String -> Map String [String] -> Int
pathCount2 start end graph = length $ paths2 end graph empty False start

testData2 :: [(Int,String)]
testData2 = [
    (36,"start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end\n"),
    (103,"dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc\n"),
    (3509,"fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW\n")
    ]

test2 :: ()
test2
  | any (uncurry (/=) . fmap (pathCount2 "start" "end" . parse)) testData2 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (pathCount2 "start" "end" . parse) $ readFile "input/12.txt"
