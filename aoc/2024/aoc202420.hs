module AOC202420 where

import Data.Array(Array,assocs,bounds,inRange,(!))
import Data.Map(Map)
import qualified Data.Map
import Data.Set(Set)
import qualified Data.Set

import Debug.Trace(traceShow)

import AOC

aoc = AOC {
    day="20",
    testData=unlines [
    "###############",
    "#...#...#.....#",
    "#.#.#.#.#.###.#",
    "#S#...#.#.#...#",
    "#######.#.#.###",
    "#######.#.#...#",
    "#######.#.###.#",
    "###..E#...#...#",
    "###.#######.###",
    "#...###...#...#",
    "#.#####.#.###.#",
    "#.#...#.#.#...#",
    "#.#.#.#.#.#.###",
    "#...#...#...###",
    "###############"
    ],
    testResult="44",
    testData2="",
    testResult2="285",
    aocParse=parse2da,
    aocTest=result 2,
    aocResult=result 100,
    aocParse2=parse2da,
    aocTest2=result2 50,
    aocResult2=result2 100
    }

type Grid = Array (Int,Int) Char

getStart = fst . head . filter ((== 'S') . snd) . assocs

getEnd = fst . head . filter ((== 'E') . snd) . assocs

makeTrail :: Grid -> Map (Int,Int) Int
makeTrail grid = bfs Data.Map.empty [(end,0)]
  where
    bfs trail [] = trail
    bfs trail ((xy@(x,y),t):queue)
      -- start is only reachable from one spot in my input
      | xy == start = Data.Map.insert xy t trail
      | Data.Map.member xy trail = bfs trail queue
      | otherwise = bfs (Data.Map.insert xy t trail) (queue ++ neighbors)
      where
        neighbors = [(newxy,t+1)
                     | newxy <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)],
                       not $ Data.Map.member newxy trail, grid!newxy /= '#']
    start = getStart grid
    end = getEnd grid

findCheats :: Int -> Grid -> [(Int,(Int,Int),(Int,Int))]
findCheats threshold grid = bfs Data.Set.empty [(start,0)]
  where
    start = getStart grid
    trail = makeTrail grid
    limit = (trail Data.Map.! start) - threshold
    bfs seen [] = []
    bfs seen ((xy@(x,y),t):queue)
      | t > limit = []
      | Data.Set.member xy seen = bfs seen queue
      | otherwise = cheats ++ bfs (Data.Set.insert xy seen) (queue++neighbors)
      where
        -- no diagonal cheats possible in my input
        cheats = [(finishTime,xy,cheatXY)
                  | (wallXY,cheatXY) <-
                      [((x+1,y),(x+2,y)),((x-1,y),(x-2,y)),
                       ((x,y+1),(x,y+2)),((x,y-1),(x,y-2))],
                    grid!wallXY == '#',
                    Data.Map.member cheatXY trail,
                    finishTime <- [t + 2 + trail Data.Map.! cheatXY],
                    finishTime <= limit]
        neighbors = [(newxy,t+1)
                     | newxy <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)],
                       not $ Data.Set.member newxy seen, grid!newxy /= '#']

result threshold = length . findCheats threshold

findCheats2 :: Int -> Grid -> [(Int,(Int,Int),(Int,Int))]
findCheats2 threshold grid = bfs Data.Set.empty [(start,0)]
  where
    start = getStart grid
    trail = makeTrail grid
    limit = (trail Data.Map.! start) - threshold
    bfs seen [] = []
    bfs seen ((xy@(x,y),t):queue)
      | t > limit = []
      | Data.Set.member xy seen = bfs seen queue
      | otherwise = cheats ++ bfs (Data.Set.insert xy seen) (queue++neighbors)
      where
        cheats = [(finishTime,xy,cheatXY)
                  | dx <- [-20..20], dy <- [-20..20],
                    dt <- [abs dx + abs dy], dt <= 20,
                    cheatXY <- [(x+dx,y+dy)],
                    Data.Map.member cheatXY trail,
                    finishTime <- [t + dt + trail Data.Map.! cheatXY],
                    finishTime <= limit]
        neighbors = [(newxy,t+1)
                     | newxy <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)],
                       not $ Data.Set.member newxy seen, grid!newxy /= '#']

result2 threshold = length . findCheats2 threshold
