module AOC202216 where

import Debug.Trace(traceShow,traceShowId)

import Data.Map(Map,alter,delete,elems,filterWithKey,fromList,
                insert,member,size,toList,(!))
import qualified Data.Map

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

type Graph = Map String (Int,[(Int,String)])

parse = optimize . fromList . map (p . words) . lines
  where
    p ("Valve":valve:"has":"flow":rate:_:_:"to":_:valves) =
        (valve,(head $ parseInts rate,map (((,) 1) . filter (/= ',')) valves))
    splitPressures g = (Data.Map.map fst g,Data.Map.map snd g)

optimize :: Graph -> Graph
optimize graph =
    foldr openValve graph $ map fst $ filter irrelevant $ toList graph
  where irrelevant (valve,(flowRate,_)) = valve /= "AA" && flowRate == 0

openValve :: String -> Graph -> Graph
openValve valve graph = delete valve joinedGraph
  where
    joinedGraph = foldr join graph exits
    exits = snd $ graph!valve
    join (len,neighbor) g =
        insert neighbor (neighborFlow,joinedNeighborNeighbors) g
      where
        (neighborFlow,neighborNeighbors) = graph!neighbor
        joinedNeighborNeighbors :: [(Int,String)]
        joinedNeighborNeighbors =
            foldr join1 (filter ((/= valve) . snd) neighborNeighbors) exits
        join1 (len1,v1) nexits
          | v1 == neighbor = nexits
          | otherwise = join2 nexits
          where
            join2 [] = [(len+len1,v1)]
            join2 ((len2,v2):rest)
              | v2 == v1 = (min len2 (len+len1),v1):rest
              | otherwise = (len2,v2):join2 rest

pressure :: Graph -> Int
pressure = sum . map fst . elems

type Path = (Int,Int,String,Graph) -- (cost,time,location,graph)
type State = (String,Graph)

search :: Int -> Graph -> Int
search timeLimit initialGraph = timeLimit*initialPressure - finalCost
  where
    initialPressure = pressure initialGraph
    (finalCost,_,_,_) =
        maybe (0,undefined,undefined,undefined) id $
            astar heuristic neighbors toState done initialPaths

    initialPaths :: [Path]
    initialPaths = [(dt*initialPressure,dt,dest,graph)
                    | (dt,dest) <- snd $ initialGraph!"AA"]
      where graph = openValve "AA" initialGraph

    heuristic :: Path -> Int
    heuristic (cost,_,_,_) = cost

    neighbors :: Path -> [Path]
    neighbors (cost,time,valve,graph)
      | size graph == 1 = [(cost+p,time+1,valve,openedGraph)]
      | otherwise =
            [(cost+p+dt*po,time+1+dt,neighbor,openedGraph)
             | (nsteps,neighbor) <- snd (graph!valve),
               dt <- [min (timeLimit-1-time) nsteps]] ++
            [(cost+dt*p,time+dt,neighbor,graph)
             | (nsteps,neighbor) <- snd (graph!valve),
               dt <- [min (timeLimit-time) nsteps]]
      where
        p = pressure graph
        po = pressure openedGraph
        openedGraph = openValve valve graph

    toState :: Path -> State
    toState (_,_,valve,graph) = (valve,graph)

    done :: Path -> Bool
    done (_,time,_,graph) = time >= timeLimit || pressure graph == 0

result = search 30

divideGraph :: Int -> Graph -> (Graph,Graph)
divideGraph index graph =
    (optimize $ fromList $ map fst nodes,optimize $ fromList $ map snd nodes)
  where
    (_,nodes) = foldr divideNode (2*index,[]) $ toList graph
    divideNode (valve,(rate,exits)) (i,nodeList)
      | valve == "AA" =
          (i,((valve,(rate,exits)),(valve,(rate,exits))):nodeList)
      | i `mod` 2 == 0 =
          (i `div` 2,((valve,(0,exits)),(valve,(rate,exits))):nodeList)
      | otherwise =
          (i `div` 2,((valve,(rate,exits)),(valve,(0,exits))):nodeList)

divide :: Graph -> [(Graph,Graph)]
divide graph = [divideGraph i graph | i <- [1..2^(size graph - 2)]]

search2 timeLimit (graph1,graph2) =
    search timeLimit graph1 + search timeLimit graph2

result2 ncpu = parallelMapReduce ncpu (search2 26) maximum . divide
