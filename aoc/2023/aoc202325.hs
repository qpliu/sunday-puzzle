import Data.Map(Map,adjust,alter,keys,(!))
import qualified Data.Map
import Data.Set(Set,delete,empty,fold,fromList,insert,member,singleton,size,toList,union)
import Data.Time(getCurrentTime)

parse :: String -> Map String (Set String)
parse = addBacklinks . Data.Map.fromList . map (parseLine . words) . lines . filter (/= ':')
  where
    parseLine (node:edges) = (node,fromList edges)
    addBacklinks :: Map String (Set String) ->  Map String (Set String)
    addBacklinks graph = foldl backlink graph $ keys graph
    backlink :: Map String (Set String) -> String ->  Map String (Set String)
    backlink graph node = foldr (alter (Just . maybe (singleton node) (insert node))) graph $ toList $ graph!node

edges :: Map String (Set String) -> [(String,String)]
edges graph = [(a,b) | a <- keys graph, b <- keys graph, b > a, member b (graph!a)]

groupSize :: Map String (Set String) -> Set (String,String) -> Int
groupSize graph disconnected = size $ walk empty [head $ keys graph]
  where
    walk visited [] = visited
    walk visited (node:queue)
      | node `member` visited = walk visited queue
      | otherwise = walk (insert node visited) ((filter (not . skip) $ toList (graph!node)) ++ queue)
      where
        skip dest = member dest visited || member (min node dest,max node dest) disconnected

-- brute force is way too slow
-- it checks about 5000 triplets in a minute and
-- there are at least a half-billion of them, which would be about 70 days
result :: String -> Int
result input = m*(n-m)
  where
    graph = parse input
    es = edges graph
    n = length (keys graph)
    m = head $ dropWhile (== n) [groupSize graph (fromList [a,b,c]) | a <- es, b <- es, b > a, c <- es, c > b]

resultWithProgress :: String -> IO ()
resultWithProgress input = progress $ zip [1..] results
  where
    graph = parse input
    es = edges graph
    n = length (keys graph)
    results = [groupSize graph (fromList [a,b,c]) | a <- es, b <- es, b > a, c <- es, c > b]
    progress ((i,m):rest)
      | m /= n = print (i,m,n-m,m*(n-m))
      | i `mod` 10000 == 0 = do
          putStr (show i ++ ":")
          getCurrentTime >>= print
          progress rest
      | otherwise = progress rest

-- Karger's algorithm
resultk :: String -> Int
resultk input = max (getSizes $ k2 $ k1 $ k2 graph1) (getSizes $ k1 $ k2 $ k1 graph1) -- super hacky shuffle randomization
  where
    graph = parse input
    getSizes = product . map fst . Data.Map.elems
    graph1 = karger (Data.Map.map ((,) 1) graph) (edges graph)

-- this is very hacky way to mix up the edges
shuffle (a:b:c:d:e:f:g:h:i:j:k:l:m:rest) = (i:a:e:k:h:d:l:shuffle (m:b:j:g:f:c:rest))
shuffle a = a
k1 g = karger g (reverse $ shuffle $ edges $ Data.Map.map snd g)
k2 g = karger g (shuffle $ shuffle $ edges $ Data.Map.map snd g)

karger :: Map String (Int,(Set String)) -> [(String,String)] -> Map String (Int,(Set String))
karger graph edgeList = contract graph edgeList
  where
    contract g [] = g
    contract g ((n1,n2):es)
      | Data.Map.size g == 2 = g
      | not (Data.Map.member n1 g) || not (Data.Map.member n2 g) = contract g es
      | otherwise = contract (merge g n1 n2) es
    merge g n1 n2 = adjust updateN1 n1 $ Data.Map.delete n2 $ fold (adjust updateToN1) g n2links
      where
        (n2count,n2linksPlusN1) = g!n2
        n2links = delete n1 n2linksPlusN1
        updateToN1 (nodecount,nodelinks) = (nodecount,insert n1 $ delete n2 nodelinks)
        updateN1 (n1count,n1links) = (n1count+n2count,union (delete n2 n1links) n2links)

testData :: String
testData = unlines [
    "jqt: rhn xhk nvd",
    "rsh: frs pzl lsr",
    "xhk: hfx",
    "cmg: qnr nvd lhk bvb",
    "rhn: xhk bvb hfx",
    "bvb: xhk hfx",
    "pzl: lsr hfx nvd",
    "qnr: nvd",
    "ntq: jqt hfx bvb xhk",
    "nvd: lhk",
    "lsr: lhk",
    "rzs: qnr cmg lsr rsh",
    "frs: qnr lhk lsr"
    ]

test :: ()
test
  | result testData /= 54 = error "a"
  | resultk testData /= 54 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap resultk $ readFile "input/25.txt"

-- 431013 is too small
-- 492633 is too small
-- 619225 is the right answer

result2 :: String -> Int
result2 = undefined

test2 :: ()
test2
  | result2 testData /= 47 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/25.txt"
