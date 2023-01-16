{-
--- Day 14: Space Stoichiometry ---

As you approach the rings of Saturn, your ship's low fuel indicator turns on.
There isn't any fuel here, but the rings have plenty of raw material. Perhaps
your ship's Inter-Stellar Refinery Union brand nanofactory can turn these raw
materials into fuel.

You ask the nanofactory to produce a list of the reactions it can perform that
are relevant to this process (your puzzle input). Every reaction turns some
quantities of specific input chemicals into some quantity of an output
chemical. Almost every chemical is produced by exactly one reaction; the only
exception, ORE, is the raw material input to the entire process and is not
produced by a reaction.

You just need to know how much ORE you'll need to collect before you can
produce one unit of FUEL.

Each reaction gives specific quantities for its inputs and output; reactions
cannot be partially run, so only whole integer multiples of these quantities
can be used. (It's okay to have leftover chemicals when you're done, though.)
For example, the reaction 1 A, 2 B, 3 C => 2 D means that exactly 2 units of
chemical D can be produced by consuming exactly 1 A, 2 B and 3 C. You can run
the full reaction as many times as necessary; for example, you could produce
10 D by consuming 5 A, 10 B, and 15 C.

Suppose your nanofactory produces the following list of reactions:

| 10 ORE => 10 A
| 1 ORE => 1 B
| 7 A, 1 B => 1 C
| 7 A, 1 C => 1 D
| 7 A, 1 D => 1 E
| 7 A, 1 E => 1 FUEL

The first two reactions use only ORE as inputs; they indicate that you can
produce as much of chemical A as you want (in increments of 10 units, each 10
costing 10 ORE) and as much of chemical B as you want (each costing 1 ORE). To
produce 1 FUEL, a total of 31 ORE is required: 1 ORE to produce 1 B, then 30
more ORE to produce the 7 + 7 + 7 + 7 = 28 A (with 2 extra A wasted) required
in the reactions to convert the B into C, C into D, D into E, and finally E
into FUEL. (30 A is produced because its reaction requires that it is created
in increments of 10.)

Or, suppose you have the following list of reactions:

| 9 ORE => 2 A
| 8 ORE => 3 B
| 7 ORE => 5 C
| 3 A, 4 B => 1 AB
| 5 B, 7 C => 1 BC
| 4 C, 1 A => 1 CA
| 2 AB, 3 BC, 4 CA => 1 FUEL

The above list of reactions requires 165 ORE to produce 1 FUEL:

 - Consume 45 ORE to produce 10 A.
 - Consume 64 ORE to produce 24 B.
 - Consume 56 ORE to produce 40 C.
 - Consume 6 A, 8 B to produce 2 AB.
 - Consume 15 B, 21 C to produce 3 BC.
 - Consume 16 C, 4 A to produce 4 CA.
 - Consume 2 AB, 3 BC, 4 CA to produce 1 FUEL.

Here are some larger examples:

 - 13312 ORE for 1 FUEL:
   | 157 ORE => 5 NZVS
   | 165 ORE => 6 DCFZ
   | 44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
   | 12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
   | 179 ORE => 7 PSHF
   | 177 ORE => 5 HKGWZ
   | 7 DCFZ, 7 PSHF => 2 XJWVT
   | 165 ORE => 2 GPVTF
   | 3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT

 - 180697 ORE for 1 FUEL:
   | 2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
   | 17 NVRVD, 3 JNWZP => 8 VPVL
   | 53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
   | 22 VJHF, 37 MNCFX => 5 FWMGM
   | 139 ORE => 4 NVRVD
   | 144 ORE => 7 JNWZP
   | 5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
   | 5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
   | 145 ORE => 6 MNCFX
   | 1 NVRVD => 8 CXFTF
   | 1 VJHF, 6 MNCFX => 4 RFSQX
   | 176 ORE => 6 VJHF

 - 2210736 ORE for 1 FUEL:
   | 171 ORE => 8 CNZTR
   | 7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
   | 114 ORE => 4 BHXH
   | 14 VRPVC => 6 BMBT
   | 6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
   | 6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
   | 15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
   | 13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
   | 5 BMBT => 4 WPTQ
   | 189 ORE => 9 KTJDG
   | 1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
   | 12 VRPVC, 27 CNZTR => 2 XDBXC
   | 15 KTJDG, 12 BHXH => 5 XCVML
   | 3 BHXH, 2 VRPVC => 7 MZWV
   | 121 ORE => 7 VRPVC
   | 7 XCVML => 6 RJRHP
   | 5 BHXH, 4 VRPVC => 5 LTCX

Given the list of reactions in your puzzle input, what is the minimum amount
of ORE required to produce exactly 1 FUEL?
-}

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
