module AOC202216 where

import Data.Array(Array,array,bounds,indices,(!))
import Data.Bits(complement,popCount,setBit,testBit,(.|.))
import Data.List(sort)
import Data.Map(Map,fromList,insert,member,singleton,toList)
import qualified Data.Map
import Data.Tuple(swap)

import AOC

aoc = AOC {
    day="../../2022/input/16",
    aocTests=[
        AOCTest {
            testData=unlines [
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
                ],
            testResult=Just "1651",
            testResult2=Just "1707"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=const result,
        pcodeTest2=result2,
        pcodeResult=const result,
        pcodeResult2=result2
        }
    }

type Graph = Array Int (Int,[(Int,Int)])

parse = toGraph . removeZeros . toEdges . map (p . words) . lines
  where
    p ("Valve":valve:"has":"flow":rate:_:_:"to":_:valves) =
        (valve,(head $ parseInts rate,map (filter (/= ',')) valves))

toEdges :: [(String,(Int,[String]))] -> [(String,(Int,[(String,Int)]))]
toEdges valves = map toE valves
  where
    graph1 = fromList $ map (fmap snd) valves
    toE (valve,(flowRate,tunnels)) =
        (valve,(flowRate,walk (singleton valve 0) $ map ((,) 1) tunnels))
    walk edges [] = filter ((/= 0) . snd) $ toList edges
    walk edges ((dist,dest):queue)
      | member dest edges && dist > edges Data.Map.! dest = walk edges queue
      | otherwise =
          walk (insert dest dist edges)
               (queue ++ map ((,) (dist+1)) (graph1 Data.Map.! dest))

removeZeros :: [(String,(Int,[(String,Int)]))] -> [(String,(Int,[(String,Int)]))]
removeZeros valves = map (fmap (fmap rm)) $ toList kept
  where
    kept = fromList $ filter keep valves
    keep (name,(flowRate,_)) = flowRate /= 0 || name == "AA"
    rm = filter ((`member` kept) . fst)

toGraph :: [(String,(Int,[(String,Int)]))] -> Graph
toGraph valves = array (0,length valves - 1) $ map toG valves
  where
    index = ((fromList $ map (swap . fmap fst) $ zip [0..] $ sort valves) Data.Map.!)
    toG (name,(flowRate,tunnels)) =
        (index name,(flowRate,map (fmap index . swap) tunnels))

type Path = ((Int,Int,Int),Int) -- (State,released)
type State = (Int,Int,Int) -- (timeLeft,openValves,location)

search :: Int -> Int -> Graph -> Int
search timeLimit openValves graph = finalReleased
  where
    Just (_,finalReleased) = 
        astar heuristic neighbors toState done initialPaths

    heuristic ((timeLeft,openValves,_),released) =
      - released - sum [fst (graph!i)*timeLeft | i <- indices graph,
                                                 not (testBit openValves i)]

    toState = fst

    done ((timeLeft,openValves,location),_) = timeLeft == 0

    initialPaths = [((timeLimit,openValves,0),0)]

    neighbors ((timeLeft,openValves,location),released)
      | null moves = [((0,openValves,location),released)]
      | otherwise = moves
      where
        moves = [move dist dest | (dist,dest) <- snd (graph!location),
                                  dist+1 < timeLeft,
                                  not (testBit openValves dest)]
        move dist dest = ((newTimeLeft,newOpenValves,dest),newReleased)
          where
            newOpenValves = setBit openValves dest
            newTimeLeft = timeLeft-dist-1
            newReleased = fst (graph!dest)*newTimeLeft + released

result = search 30 1

search2 graph openValves =
    search 26 openValves1 graph + search 26 openValves2 graph
  where
    openValves1 = openValves .|. 1
    openValves2 = complement openValves .|. 1

result2 ncpu graph =
    parallelMapReduce ncpu (search2 graph) maximum
                      [4*i | i <- [0..2^(maxValve-1)-1],
                             abs (popCount i - (maxValve `div` 2)) < 2]
  where
    (_,maxValve) = bounds graph
