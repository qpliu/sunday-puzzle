import Debug.Trace(traceShow)

import Data.List(sort)
import Data.Map(Map,keysSet,toList,(!))
import qualified Data.Map
import Data.Set(Set,difference,elems,empty,fromList,insert,maxView,member,singleton,size,union)
import qualified Data.Set

type State = (Int,Int,String,String,Set String) -- time left,pressure,current valve,last valve,opened -- last valve to prevent pointless backtracking, does not eliminate pointless loops though
type Cave = (Map String Int,Map String [String]) -- (flows,tunnels)

parse :: String -> Cave
parse input = (flows,tunnels)
  where
    valves = Data.Map.fromList $ map (p . words) $ lines input
    flows = Data.Map.map fst valves
    tunnels = Data.Map.map snd valves

    p ("Valve":valve:"has":"flow":rate:"tunnels":"lead":"to":"valves":tunnels) = (valve,(pRate rate,map pTunnel tunnels))
    p ("Valve":valve:"has":"flow":rate:"tunnel":"leads":"to":"valve":tunnels) = (valve,(pRate rate,map pTunnel tunnels))
    pRate = read . filter (/= ';') . drop 5
    pTunnel = filter (/= ',')

nextStates :: Cave -> State -> [State]
nextStates (flows,tunnels) (timeLeft,pressure,valve,lastValve,opened)
  | member valve opened || flows!valve <= 0 = moves
  | otherwise = (timeLeft-1,newPressure,valve,valve,insert valve opened) : moves
  where
    newPressure = pressure + sum [flows!v | v <- elems opened]
    moves = [(timeLeft-1,newPressure,next,valve,opened) | next <- tunnels!valve, next /= lastValve]

search1 :: Cave -> State -> Int
search1 cave@(flows,_) initialState = astar $ singleton (h initialState,initialState)
  where
    maxFlow = sum flows

    h (timeLeft,pressure,_,_,_) = pressure+timeLeft*maxFlow

    astar openQ
      | size openQ `mod` 5000 == 0 && traceShow (size openQ,timeLeft,pressure) False = undefined
      | timeLeft <= 0 = pressure
      | otherwise = astar $ foldr add newOpenQ $ nextStates cave current
      where
        Just ((_,current@(timeLeft,pressure,_,_,opened)),newOpenQ) = maxView openQ
        add state open = insert (h state,state) open

run1 :: String -> Int
run1 input = search1 cave (30,0,"AA","AA",empty)
  where cave = parse input  

testData :: String
testData = unlines [
    "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB",
    "Valve BB has flow rate=13; tunnels lead to valves CC, AA",
    "Valve CC has flow rate=2; tunnels lead to valves DD, BB",
    "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE",
    "Valve EE has flow rate=3; tunnels lead to valves FF, DD",
    "Valve FF has flow rate=0; tunnels lead to valves EE, GG",
    "Valve GG has flow rate=0; tunnels lead to valves FF, HH",
    "Valve HH has flow rate=22; tunnel leads to valve GG",
    "Valve II has flow rate=0; tunnels lead to valves AA, JJ",
    "Valve JJ has flow rate=21; tunnel leads to valve II"
    ]

test :: ()
test
  | run1 testData /= 1651 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap run1 $ readFile "input/16.txt"

type State2 = (Int,Int,String,String,String,String,Set String) -- time left,pressure,current valve,last valve,current elephant valve,last elephant valve,opened

nextStates2 :: Cave -> State2 -> [State2]
nextStates2 (flows,tunnels) (timeLeft,pressure,valve,lastValve,valveE,lastValveE,opened)
  | (member valve opened || flows!valve <= 0) && (member valveE opened || flows!valveE <= 0) = moves
  | (member valve opened || flows!valve <= 0) = openE ++ moves
  | (member valveE opened || flows!valveE <= 0) = openMe ++ moves
  | valve == valveE = openMe ++ openE ++ moves
  | otherwise = openBoth : openMe ++ moves
  where
    newPressure = pressure + sum [flows!v | v <- elems opened]
    openMe = [(timeLeft-1,newPressure,valve,valve,nextE,valveE,insert valve opened) | nextE <- tunnels!valveE, nextE /= lastValveE]
    openE = [(timeLeft-1,newPressure,next,valve,valveE,valveE,insert valveE opened) | next <- tunnels!valve, next /= lastValve]
    moves = [(timeLeft-1,newPressure,next,valve,nextE,valveE,opened) | next <- tunnels!valve, nextE <- tunnels!valveE, next /= lastValve, nextE /= lastValveE, valve /= valveE || next >= nextE]
    openBoth = (timeLeft-1,newPressure,valve,valve,valveE,valveE,insert valve (insert valveE opened))

-- This is way too slow.  Took hours to get the answer.
search2 :: Cave -> State2 -> Int
search2 cave@(flows,_) initialState = astar $ singleton (h initialState,initialState)
  where
    maxFlow = sum flows

    h (timeLeft,pressure,_,_,_,_,opened) = pressure+timeLeft*sum [flows!v | v <- elems opened]+ sum [(timeLeft - dt2)*flow  | (dt2,flow) <- zip [0..] (reverse (sort [flow | (v,flow) <- toList flows, not (member v opened)])), dt2 < timeLeft]

    astar openQ
      | size openQ `mod` 5000 == 0 && traceShow (size openQ,timeLeft,pressure) False = undefined
      | timeLeft <= 0 = pressure
      | otherwise = astar $ foldr add newOpenQ $ nextStates2 cave current
      where
        Just ((_,current@(timeLeft,pressure,_,_,_,_,_)),newOpenQ) = maxView openQ
        add state open = insert (h state,state) open

run2 :: String -> Int
run2 input = search2 cave (26,0,"AA","AA","AA","AA",empty)
  where cave = parse input  

test2 :: ()
test2
  | run2 testData /= 1707 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap run2 $ readFile "input/16.txt"
