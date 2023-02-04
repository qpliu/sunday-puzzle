-- Parsing creates a directed acyclic graph (DAG) from ORE to FUEL.

-- Build DAG from FUEL to ORE, each edge has undetermined value.
-- For each node, once all incoming edges have known values, the values of
-- its outgoing edges can be determined and propagated.  The value of the
-- initial incoming edge is 1 to FUEL.

import Control.Monad.State(State,get,put,runState)
import Data.Map(Map,alter,empty,foldWithKey,fromList,insert,keys,member,(!))
import qualified Data.Map

parse :: String -> Map String (Int,[(String,Int)])
parse = fromList . p [] . map words . lines . filter (/= ',') 
  where
    p inputs (("=>":n:output:_):rest) = (output,(read n,inputs)) : p [] rest
    p inputs ((n:input:more):rest) = p ((input,read n):inputs) (more:rest)
    p _ _ = []

invertDAG :: Map String (Int,[(String,Int)]) -> Map String [String]
invertDAG dag = foldWithKey buildIDAG empty dag
  where
    initialIDAG = fromList $ zip (keys dag) $ repeat []
    buildIDAG output (_,inputs) idag = foldr (buildIDAGedge output) idag inputs
    buildIDAGedge output (input,_) idag = alter (Just . maybe [output] (output:)) input idag

type Needs = State (Map String [String],Map String Int)

findNeeds :: Int -> String -> Map String (Int,[(String,Int)]) -> Map String Int
findNeeds finalQuantity finalOutput reactions = snd $ snd $ runState (produce finalQuantity finalOutput) ((invertDAG reactions),empty)
  where
    produce :: Int -> String -> Needs ()
    produce quantity output
      -- ORE is not produced by a reaction
      | not (member output reactions) = return ()

      | otherwise = do
        let (quantityPerReaction,inputs) = reactions!output
        let nreactions = (quantity + quantityPerReaction - 1) `div` quantityPerReaction
        mapM_ (\ (input,inputQuantity) -> propogateNeeds output (inputQuantity*nreactions) input) inputs

    addNeeded :: Int -> String -> Needs ()
    addNeeded quantity output = do
        (unknowns,neededQuantities) <- get
        let newNeeded = alter (Just . maybe quantity (+quantity)) output neededQuantities
        put (unknowns,newNeeded)

    propogateNeeds :: String -> Int -> String -> Needs ()
    propogateNeeds neededBy quantity output = do
        addNeeded quantity output
        (unknowns,neededQuantities) <- get
        let newUnknownList = filter (/= neededBy) (unknowns!output)
        let newUnknowns = insert output newUnknownList unknowns
        put (newUnknowns,neededQuantities)
        if null newUnknownList
          then produce (neededQuantities!output) output
          else return ()

testData :: [(Int,String)]
testData = [
    (165, "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL"),
    (13312,"157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"),
    (180697,"2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n17 NVRVD, 3 JNWZP => 8 VPVL\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n22 VJHF, 37 MNCFX => 5 FWMGM\n139 ORE => 4 NVRVD\n144 ORE => 7 JNWZP\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n145 ORE => 6 MNCFX\n1 NVRVD => 8 CXFTF\n1 VJHF, 6 MNCFX => 4 RFSQX\n176 ORE => 6 VJHF"),
    (2210736,"171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX")
    ]

run :: String -> Int
run = (!"ORE") . findNeeds 1 "FUEL" . parse

test :: ()
test
  | any (uncurry (/=) . fmap run) testData = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap run $ readFile "input/14.txt"

search :: Int -> String -> Int
search target input = findBound 10
  where
    dag = parse input
    f n = (findNeeds n "FUEL" dag)!"ORE"
    findBound n
      | f n < target = findBound (n*10)
      | otherwise = binary (n `div` 10) n
    binary low high
      | low + 1 >= high = low
      | f middle < target = binary middle high
      | otherwise = binary low middle
      where middle = (low + high) `div` 2

test2 :: ()
test2
  | search 1000000000000 (snd (testData!!1)) /= 82892753 = error "a"
  | search 1000000000000 (snd (testData!!2)) /= 5586022 = error "b"
  | search 1000000000000 (snd (testData!!3)) /= 460664 = error "c"
  | otherwise = ()

part2 :: IO Int
part2 =  fmap (search 1000000000000) $ readFile "input/14.txt"
