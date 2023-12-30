import Debug.Trace(traceShow)
import Data.List(group,sort)
import Data.Map(Map,(!))
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S
import Data.Time(diffUTCTime,getCurrentTime)
import System.Random(RandomGen,mkStdGen,randomR)

-----------------------------------------------------------------------------

parse :: Params -> String -> Parsed
parse () = addBacklinks . M.fromList . map (parseLine . words) . lines . filter (/= ':')
  where
    parseLine (node:edges) = (node,S.fromList edges)
    addBacklinks graph = foldl backlink graph $ M.keys graph
    backlink graph node = S.foldr (M.alter (Just . maybe (S.singleton node) (S.insert node))) graph $ graph!node

edges :: (Eq a, Ord a) => Map a (Set a) -> [(a,a)]
edges graph = [(a,b) | a <- M.keys graph, b <- M.keys graph, b > a, S.member b (graph!a)]

replace :: Eq a => a -> a -> a -> a
replace target replacement item
  | item == target = replacement
  | otherwise = item

-- the minimum cut for my input is mqb/qlc ssd/xqh khn/nrs

-- Karger-Stein algorithm: https://en.wikipedia.org/wiki/Karger%27s_algorithm
type VE = (Map String (Set String),Map String String,[(String,String)])

vertices :: VE -> Int
vertices (_,mergers,_) = length $ group $ sort $ M.elems mergers

contract :: RandomGen rng => (rng,VE) -> Int -> (rng,VE)
contract (rng,ve@(graph,mergers,edgeList)) target
  | vertices ve <= target = (rng,(graph,mergers,validEdges))
--  | (node1,node2) `elem` [("mqb","qlc"),("ssd","xqh"),("khn","nrs"), ("hfx","pzl"),("bvb","cmg"),("jqt","nvd")] && traceShow (length validEdges,length $ group $ sort $ M.elems mergers) False = undefined
  | otherwise = contract (newRNG,(graph,M.map (replace (mergers!node2) (mergers!node1)) mergers,newEdgeList)) target
  where
    validEdges = filter (not . selfEdge) edgeList
    selfEdge (n1,n2) = mergers!n1 == mergers!n2
    (randomN,newRNG) = randomR (0,length validEdges) rng
    ((node1,node2):newEdgeList) = uncurry (flip (++)) $ splitAt randomN validEdges

fastmincut :: RandomGen rng => Int -> (rng,VE) -> (rng,VE)
fastmincut minimumCut (rng,ve@(graph,mergers,edgeList))
  | vertices ve <= 6 = contract (rng,ve) 2
  | length edgeList1 <= minimumCut || length edgeList1 < length edgeList2 = (rng1,ve1)
  | otherwise = (rng2,ve2)
  where
    target = (1 + (10000*vertices ve) `div` 14142)
    (rng1,ve1@(_,_,edgeList1)) = fastmincut minimumCut $ contract (rng,ve) target
    (rng2,ve2@(_,_,edgeList2)) = fastmincut minimumCut $ contract (rng1,ve) target

ks :: RandomGen rng => Int -> rng -> Int -> Map String (Set String) -> ([(String,String)],[Int])
ks minimumCut rng maxTries graph
  | maxTries <= 0 = error "fail"
  | length edgeList <= minimumCut = (edgeList,partitions)
  | traceShow ("fail",maxTries,length edgeList) False = undefined
  | otherwise = ks minimumCut rng2 (maxTries-1) graph
  where
    (rng2,(_,mergers,edgeList)) = fastmincut minimumCut (rng,(graph,M.fromList $ zip (M.keys graph) (M.keys graph),edges graph))
    partitions = map length $ group $ sort $ M.elems mergers

karger :: RandomGen rng => Int -> rng -> Int -> Map String (Set String) -> ([(String,String)],[Int])
karger minimumCut rng maxTries graph
 | maxTries <= 0 = error "fail"
 | length edgeList <= minimumCut = (edgeList,partitions)
  | traceShow ("fail",maxTries,length edgeList) False = undefined
  | otherwise = karger minimumCut rng2 (maxTries-1) graph
 where
    (rng2,(_,mergers,edgeList)) = contract (rng,(graph,M.fromList $ zip (M.keys graph) (M.keys graph),edges graph)) 2
    partitions = map length $ group $ sort $ M.elems mergers

result :: Parsed -> Result
result = product . snd . ks 3 (mkStdGen 1) 100

parse2 :: Params -> String -> Parsed
parse2 = parse

result2 :: Parsed -> Result
result2 = undefined

-----------------------------------------------------------------------------
type Params = ()
type Parsed = Map String (Set String)
type Result = Int
type Result2 = Int

inputFile :: String
inputFile = "input/25.txt"

testResult :: Result
testResult = 54

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

testResult2 :: Result2
testResult2 = 1234

testData2 :: String
testData2 = testData

testParams :: Params
testParams = ()

params :: Params
params = ()
-----------------------------------------------------------------------------
test :: ()
test
  | result (parse testParams testData) /= testResult = error "a"
  | otherwise = ()

part1 :: IO ()
part1 = do
    t0 <- getCurrentTime
    r <- fmap (result . parse params) $ readFile inputFile
    print r
    t1 <- getCurrentTime
    print $ diffUTCTime t1 t0

test2 :: ()
test2
  | result2 (parse2 testParams testData2) /= testResult2 = error "a"
  | otherwise = ()

part2 :: IO ()
part2 = do
    t0 <- getCurrentTime
    r <- fmap (result2 . parse2 params) $ readFile inputFile
    print r
    t1 <- getCurrentTime
    print $ diffUTCTime t1 t0
