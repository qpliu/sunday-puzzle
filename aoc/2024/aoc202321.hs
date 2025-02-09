module AOC202321 where

import Data.Array(Array,array,assocs,bounds,inRange,(!))
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
        pcodeParse2=const parse2,
        pcodeTest=const $ result 6,
        pcodeTest2=undefined,
        pcodeResult=const $ result 64,
        pcodeResult2=result2 26501365
        }
    }

parse input = (grid,start)
  where
    grid = parse2da input
    [(start,'S')] = filter ((== 'S') . snd) $ assocs grid

step :: Array (Int,Int) Char -> Set (Int,Int) -> Set (Int,Int)
step grid locs = fromList $ concatMap nextLocs $ elems locs
  where
    nextLocs (x,y) = [xy | xy <- [(x+1,y),(x,y+1),(x-1,y),(x,y-1)],
                           inRange (bounds grid) xy, grid!xy /= '#']

result nsteps (grid,start) =
    size $ head $ drop nsteps $ iterate (step grid) $ fromList [start]

-- Like the example map, the map in my input is square with an odd side
-- length, has no rocks on the outer edges, and has the start in the middle,
-- which, ignoring rocks, is an even number of steps from each of the four
-- neighboring tiles.
--
-- Unlike the example map, the map in my input has no rocks in middle column,
-- and has no rocks in the middle in the middle row.
parse2 input
  | xmax /= ymax || odd xmax || even startX
                 || startX*2 /= xmax || startY*2 /= ymax
                 || 0 < length [() | t <- [0..xmax],
                                     grid!(t,0) /= '.'
                                       || grid!(t,ymax) /= '.'
                                       || grid!(0,t) /= '.'
                                       || grid!(xmax,t) /= '.']
                 || 0 < length [() | t <- [1..startX],
                                     grid!(startX-t,startY) /= '.'
                                       || grid!(startX+t,startY) /= '.'
                                       || grid!(startX,startY-t) /= '.'
                                       || grid!(startX,startY+t) /= '.'] =
      error "input does not fit assumptions"
  | otherwise = (grid,start)
  where
    ((0,0),(xmax,ymax)) = bounds grid
    grid = parse2da input
    [(start@(startX,startY),'S')] = filter ((== 'S') . snd) $ assocs grid

-- To simplify things, consider double tiles for the vertical and horizontal
-- strips and quadruple tiles for the corners so that the first step onto
-- each macro tile will be on an even step, eliminating the need to
-- distinguish between even and odd tiles.
macroTile :: Int -> Int -> Array (Int,Int) Char -> Array (Int,Int) Char
macroTile nx ny grid = array ((0,0),(nx*(xmax+1)-1,ny*(ymax+1)-1))
                             [((x,y),grid!(x `mod` (xmax+1),y `mod` (ymax+1)))
                              | x <- [0..nx*(xmax+1)-1],
                                y <- [0..ny*(ymax+1)-1]]
  where ((0,0),(xmax,ymax)) = bounds grid

reachCount :: Array (Int,Int) Char -> (Int,Int) -> [(Int,Int)]
reachCount grid start =
    collect $ zip [0..] $ iterate (step grid) (fromList [start])
  where
    collect (_:(t,a):rest@(_:(_,b):_))
      | a == b = [(t,size a)]
      | otherwise = (t,size a) : collect rest

centerCount :: Array (Int,Int) Char -> (Int,Int) -> Int
centerCount grid = snd . last . reachCount grid

stripCount :: Array (Int,Int) Char -> (Int,Int) -> Int -> Int -> Int
stripCount grid start len nsteps =
    fullyReachable*fullCount
        + sum [reach | n <- [0..reachable-fullyReachable-1],
                       t <- [nsteps - (n+fullyReachable)*len],
                       Just reach <- [lookup t counts]]
  where
    counts = reachCount grid start
    (fullSteps,fullCount) = last counts
    reachable = nsteps `div` len + 1
    fullyReachable = (nsteps - fullSteps) `div` len + 1

cornerCount :: Array (Int,Int) Char -> (Int,Int) -> Int -> Int -> Int
cornerCount grid start len nsteps =
    fullyReachable*(fullyReachable+1)*fullCount `div` 2
        + sum [reach*(fullyReachable+n+1)
               | n <- [0..reachable-fullyReachable-1],
                 t <- [nsteps - (n+fullyReachable)*len],
                 Just reach <- [lookup t counts]]
  where
    counts = reachCount grid start
    (fullSteps,fullCount) = last counts
    reachable = nsteps `div` len + 1
    fullyReachable = (nsteps - fullSteps) `div` len + 1

result2 nsteps ncpu (grid,start@(startX,startY)) =
    stripCount vGrid (startX,0) len (nsteps-(startY+1))
        + stripCount vGrid (startX,len-1) len (nsteps-(startY+1))
        + stripCount hGrid (0,startY) len (nsteps-(startX+1))
        + stripCount hGrid (len-1,startY) len (nsteps-(startX+1))
        + cornerCount quadGrid (len-1,len-1) len (nsteps-2*(startX+1))
        + cornerCount quadGrid (0,len-1) len (nsteps-2*(startX+1))
        + cornerCount quadGrid (len-1,0) len (nsteps-2*(startX+1))
        + cornerCount quadGrid (0,0) len (nsteps-2*(startX+1))
        + centerCount grid start
  where
    (_,(xmax,_)) = bounds grid
    len = 2*(xmax+1)
    hGrid = macroTile 2 1 grid
    vGrid = macroTile 1 2 grid
    quadGrid = macroTile 2 2 grid

{-
-- Corner sectors for my input should be
-- 148179519028649,148179508711355,148179519837816,148179521860824
-- Middle sectors for my input should be
-- 1464953710,1464953671,1464953685,1464953646
-- Center for my input should be
-- 7226
-- Total should be
-- 592723929260582
-}
