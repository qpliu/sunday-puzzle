module AOC202321 where

import Debug.Trace(traceShow)

import Data.Array(Array,assocs,bounds,inRange,(!))
import Data.Set(Set,elems,fromList,member,size)
import Data.Map(Map,keys)
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2023/input/21",
    aocTests=[
        AOCTest {
            testData=unlines [
                "...........",
                ".....###.#.",
                ".###.##..#.",
                "..#.#...#..",
                "....#.#....",
                ".##..S####.",
                ".##..#...#.",
                ".......##..",
                ".##.#.####.",
                ".##..##.##.",
                "..........."
                ],
            testResult=Just "16",
            testResult2=Nothing
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=const (result 6),
        pcodeTest2=runTest2 5000,
        pcodeResult=const (result 64),
        pcodeResult2=result2 26501365
        }
    }

parse input = (grid,start)
  where
    grid = parse2da input
    start = fst $ head $ filter ((== 'S') . snd) $ assocs grid

step :: Array (Int,Int) Char -> Set (Int,Int) -> Set (Int,Int)
step grid locs = fromList $ concatMap nextLocs $ elems locs
  where
    nextLocs (x,y) = [xy | xy <- [(x+1,y),(x,y+1),(x-1,y),(x,y-1)],
                           inRange (bounds grid) xy, grid!xy /= '#']

result nsteps (grid,start) =
    size $ head $ drop nsteps $ iterate (step grid) $ fromList [start]

-- Like the example map, the map in my input is square with an odd side
-- length, has no rocks on the outer edges, and has the start in the middle.
--
-- Unlike the example map, the map in my input has no rocks in middle column,
-- and has no rocks in the middle in the middle row.

-- There is something wrong with this code that I can't figure out.
-- It gives the wrong answer.
-- Corner sectors for my input should be
-- 148179519028649,148179508711355,148179519837816,148179521860824
-- Middle sectors for my input should be
-- 1464953710,1464953671,1464953685,1464953646
sector :: Bool -> (Int,Int) -> (Array (Int,Int) Char,(Int,Int),Int) -> Int
sector isCorner startXY (grid,start,nsteps)
  | isCorner && odd filledReps =
        (halfFilled+1)*(halfFilled+2)*(reachable Data.Map.!fillSteps)
        + halfFilled*(halfFilled+1)*(reachable Data.Map.!(fillSteps-1))
        + sum [(i+1)*reachable Data.Map.! (nsteps - i*(side+1) - startSteps)
               | i <- [filledReps+1..reachedReps]]
  | isCorner =
        halfFilled*(halfFilled+1)*(reachable Data.Map.!fillSteps)
        + halfFilled*(halfFilled+1)*(reachable Data.Map.!(fillSteps-1))
        + sum [(i+1)*reachable Data.Map.! (nsteps - i*(side+1) - startSteps)
               | i <- [filledReps+1..reachedReps]]
  | odd filledReps =
        (halfFilled+1)*(reachable Data.Map.!(fillSteps-1))
        + halfFilled*(reachable Data.Map.!(fillSteps))
        + sum [reachable Data.Map.! (nsteps - i*(side+1) - startSteps)
               | i <- [filledReps..reachedReps]]
  | otherwise =
        halfFilled*(reachable Data.Map.!(fillSteps-1))
        + halfFilled*(reachable Data.Map.!(fillSteps))
        + sum [reachable Data.Map.! (nsteps - i*(side+1) - startSteps)
               | i <- [filledReps..reachedReps]]
  where
    (_,(side,_)) = bounds grid
    reachable =
        Data.Map.fromList $ zip [0..] $ takeUntilFilled $ map size
                          $ iterate (step grid) $ fromList [startXY]
      where
        takeUntilFilled (a:rest@(b:c:d:_))
          | a == c && b == d = [a,b]
          | otherwise = a:takeUntilFilled rest
    startSteps
      | isCorner = 2 + side
      | otherwise = 1 + side `div` 2
    fillSteps = maximum $ keys reachable

    filledReps = 1 + (nsteps-startSteps-fillSteps) `div` (side+1)
    halfFilled = filledReps `div` 2
    reachedReps = (nsteps-startSteps) `div` (side+1)

-- Center sector for my input should be 7226
center (grid,startXY,nsteps)
  | odd nsteps == odd fillSteps = reachable Data.Map.!fillSteps
  | otherwise = reachable Data.Map.!(fillSteps-1)
  where
    reachable =
        Data.Map.fromList $ zip [0..] $ takeUntilFilled $ map size
                          $ iterate (step grid) $ fromList [startXY]
      where
        takeUntilFilled (a:rest@(b:c:d:_))
          | a == c && b == d = [a,b]
          | otherwise = a:takeUntilFilled rest
    fillSteps = maximum $ keys reachable

-- should be 592723929260582
result2 nsteps ncpu (grid,start) =
    parallelMapReduce ncpu ($ (grid,start,nsteps)) sum
        [sector True (0,0),         sector True (side,0),
         sector True (0,side),      sector True (side,side),
         sector False (0,halfSide), sector False (side,halfSide),
         sector False (halfSide,0), sector False (halfSide,side),
         center]
  where
    (_,(side,_)) = bounds grid
    halfSide = side `div` 2

runTest2 = undefined
