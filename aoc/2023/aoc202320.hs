import Data.Map(Map,adjust,fromList,insert,keys,member,toList,(!))
import Data.Set(Set,union)
import qualified Data.Set

data Module =
    Broadcast [String]
  | FlipFlop Bool [String] [String]
  | Conjunction (Map String Bool) [String]
  deriving (Eq,Show)

parse :: String -> Map String Module
parse = fromList . addInputs . map (parseModule . words) . lines
  where
    parseModule ("broadcaster":"->":dests) = ("broadcaster",Broadcast (map (filter (/= ',')) dests))
    parseModule (('%':name):"->":dests) = (name,FlipFlop False [] (map (filter (/= ',')) dests))
    parseModule (('&':name):"->":dests) = (name,Conjunction (fromList []) (map (filter (/= ',')) dests))

destinations :: Module -> [String]
destinations (Broadcast dests) = dests
destinations (FlipFlop _ _ dests) = dests
destinations (Conjunction _ dests) = dests

sources :: Module -> [String]
sources (Broadcast dests) = []
sources (FlipFlop _ inputs dests) = inputs
sources (Conjunction inputs dests) = keys inputs

addInputs :: [(String,Module)] -> [(String,Module)]
addInputs mods = map add mods
  where
    inputTable = foldl collect (fromList $ map (fmap (const [])) mods) mods
    collect table (src,modul) = foldl (collectSrc src) table (destinations modul)
    collectSrc src table dest = adjust (src:) dest table

    add (name,(FlipFlop _ _ dests)) = (name,FlipFlop False (inputTable!name) dests)
    add (name,(Conjunction _ dests)) = (name,Conjunction (fromList [(input,False) | input <- inputTable!name]) dests)
    add assoc = assoc

press :: (((Int,Int),Bool),Map String Module) -> (((Int,Int),Bool),Map String Module)
press (initialCounts,initialState) = process initialCounts [("button","broadcaster",False)] initialState
  where
    process counts [] state = (counts,state)
    process ((nlow,nhigh),rx) ((srcName,name,pulse):queue) state
      | not (member name state) = process newCounts queue state
      | otherwise = process newCounts (queue ++ output) (insert name newModule state)
      where
        newCounts | pulse = ((nlow,nhigh+1),rx) | otherwise = ((nlow+1,nhigh),rx || name == "rx")
        (output,newModule) = processModule (state!name)
        processModule modul@(Broadcast dests) = ([(name,dest,pulse) | dest <- dests],modul)
        processModule modul@(FlipFlop st inputs dests)
          | pulse = ([],modul)
          | otherwise = ([(name,dest,not st) | dest <- dests],FlipFlop (not st) inputs dests)
        processModule (Conjunction inputs dests) = ([(name,dest,not (and newInputs)) | dest <- dests],Conjunction newInputs dests)
          where newInputs = insert srcName pulse inputs

result :: String -> Int
result = uncurry (*) . fst . fst . head . drop 1000 . iterate press . (,) ((0,0),False) . parse

testData :: [String]
testData = map unlines [[
    "broadcaster -> a, b, c",
    "%a -> b",
    "%b -> c",
    "%c -> inv",
    "&inv -> a"],[
    "broadcaster -> a",
    "%a -> inv, con",
    "&inv -> b",
    "%b -> con",
    "&con -> output"
    ]]

test :: ()
test
  | map result testData /= [32000000,11687500] = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/20.txt"

bfresult2 :: String -> Int
bfresult2 = fst . head . dropWhile noRx . zip [0..] . iterate press . (,) ((0,0),False) . parse
  where noRx (_,(((_),rx),_)) = not rx

-- In my input: 
-- rx <- &gf <- &kr, &zs, &kf, &qk

-- broadcaster -> kg dz ff bq
-- the subgraphs from broadcaster to kr zs kf qk are disjoint
-- kg -> kf -- 3767
-- dz -> kr -- 3761
-- ff -> zs -- 4091
-- bq -> qk -- 4001
-- the LCM is 231897990075517

subgraph :: String -> String -> String -> String
subgraph broadcaster rx = unlines . map f . lines
  where
    f "broadcaster -> kg, dz, ff, bq" = "broadcaster -> " ++ broadcaster
    f line@('&':a:b:" -> gf")
      | [a,b] == rx = "&" ++ rx ++ " -> gf"
      | otherwise = line ++ "stop"
    f line = line

result2 :: String -> Int
result2 input = foldl lcm 1 $ [bfresult2 $ subgraph broadcaster rx input | (broadcaster,rx) <- [("kg","kf"),("dz","kr"),("ff","zs"),("bq","qk")]]

test2 :: ()
test2
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/20.txt"
