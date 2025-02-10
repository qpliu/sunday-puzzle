module AOC202420 where

import Data.Array(Array,assocs,bounds,(!))
import Data.IntMap(IntMap,fromList)
import qualified Data.IntMap

import AOC

aoc = AOC {
    day="20",
    aocTests=[
        AOCTest {
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
            testResult=Just "44",
            testResult2=Just "285"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result 2 2,
        pcodeTest2=result 50 20,
        pcodeResult=result 100 2,
        pcodeResult2=result 100 20
        }
    }

makeTrail :: Int -> Array (Int,Int) Char -> [(Int,Int)]
makeTrail width grid = walk start 0 (0,0)
  where
    [(start,'S')] = filter ((== 'S') . snd) $ assocs grid
    [(end,'E')] = filter ((== 'E') . snd) $ assocs grid
    walk xy@(x,y) t dxy
      | xy == end = [(x+width*y,t)]
      | otherwise = (x+width*y,t) : walk nxy (t+1) ndxy
      where [(nxy,ndxy)] = [((x+dx,y+dy),ndxy)
                            | ndxy@(dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)],
                            dxy /= (-dx,-dy),
                            grid!(x+dx,y+dy) /= '#']

parse input = (width,makeTrail width grid)
  where
    grid = parse2da input
    ((0,0),(xmax,_)) = bounds grid
    width = xmax+21 -- don't let cheats wrap around

countCheats :: IntMap Int -> [(Int,Int)] -> (Int,Int) -> Int
countCheats trail cheats (xy,t) =
    length $ [() | (dxy,dt) <- cheats,
                   Just t2 <- [Data.IntMap.lookup (xy+dxy) trail],
                   t + dt <= t2]

makeCheats :: Int -> Int -> Int -> [(Int,Int)]
makeCheats threshold cheatSize width =
    [(dx+width*dy,threshold + dt)
     | dx <- [-cheatSize..cheatSize], dy <- [-cheatSize..cheatSize],
       dt <- [abs dx + abs dy], dt > 1, dt <= cheatSize]

result :: Int -> Int -> Int -> (Int,[(Int,Int)]) -> Int
result threshold cheatSize ncpu (width,trail) =
    parallelMapReduce ncpu (countCheats table cheats) sum trail
  where
    cheats = makeCheats threshold cheatSize width 
    table = fromList trail
