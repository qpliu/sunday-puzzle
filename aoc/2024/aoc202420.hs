module AOC202420 where

import Data.Array(Array,assocs)
import qualified Data.Array
import Data.Map(Map,fromList,member,(!))

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
    aocTest=result 2 cheats1,
    aocResult=result 100 cheats1,
    aocParse2=parse2da,
    aocTest2=result 50 cheats2,
    aocResult2=result 100 cheats2
    }

type Grid = Array (Int,Int) Char

getStart = fst . head . filter ((== 'S') . snd) . assocs

getEnd = fst . head . filter ((== 'E') . snd) . assocs

makeTrail :: Grid -> [((Int,Int),Int)]
makeTrail grid = walk (getStart grid) 0 (0,0)
  where
    end = getEnd grid
    walk xy@(x,y) t dxy
      | xy == end = [(xy,t)]
      | otherwise = (xy,t) : walk nxy (t+1) ndxy
      where [(nxy,ndxy)] = [((x+dx,y+dy),ndxy)
                            | ndxy@(dx,dy) <- [(1,0),(-1,0),(0,1),(0,-1)],
                            dxy /= (-dx,-dy),
                            grid Data.Array.! (x+dx,y+dy) /= '#']

countCheats :: Int -> [(Int,Int)] -> Map (Int,Int) Int -> ((Int,Int),Int)
            -> Int
countCheats threshold cheatTargets trail ((x,y),t) =
    length $ [() | (dx,dy) <- cheatTargets,
                   member (x+dx,y+dy) trail,
                   trail!(x+dx,y+dy) >= t + abs dx + abs dy + threshold]

cheats1 :: [(Int,Int)]
cheats1 =
    [(2,0),(0,2),(-2,0),(0,-2)] -- diagonal cheats are not possible in my input

cheats2 :: [(Int,Int)]
cheats2 = [(dx,dy) | dx <- [-20..20], dy <- [-20..20], abs dx + abs dy <= 20]

result threshold cheatTargets grid =
    sum $ map (countCheats threshold cheatTargets (fromList trail)) trail
  where trail = makeTrail grid
