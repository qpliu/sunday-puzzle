module AOC202321 where

import Data.Array(assocs,bounds,(!))
import Data.Bits(popCount,shiftL,shiftR,testBit,(.|.),(.&.))

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

parse :: String -> ([Integer],(Int,Int),Int)
parse input = (bits,start,xmax+1)
  where
    grid = parse2da input
    [(start,'S')] = filter ((== 'S') . snd) $ assocs grid
    ((0,0),(xmax,ymax)) = bounds grid
    bits = [sum [2^x | x <- [0..xmax], grid!(x,y) /= '#'] | y <- [0..ymax]]

startBits :: (Int,Int) -> Int -> [Integer]
startBits (startX,startY) len =
    [if y == startY then 2^startX else 0 | y <- [0..len-1]]

step :: [Integer] -> [Integer] -> [Integer]
step garden locs = zipWith (.&.) garden next
  where
    up = 0:locs
    down = drop 1 locs ++ [0]
    left = map (`shiftL` 1) locs
    right = map (`shiftR` 1) locs
    next = zipWith (.|.) (zipWith (.|.) up down) (zipWith (.|.) left right)

result nsteps (garden,start,len) =
    sum $ map popCount $ head $ drop nsteps
        $ iterate (step garden) $ startBits start len

-- Like the example map, the map in my input is square with an odd side
-- length, has no rocks on the outer edges, and has the start in the middle,
-- which, ignoring rocks, is an even number of steps from each of the four
-- neighboring tiles.
--
-- Unlike the example map, the map in my input has no rocks in middle column,
-- and has no rocks in the middle in the middle row.
parse2 input
  | even startX || startX /= startY || len /= startX*2+1
                || len /= length garden
                || head garden /= 2^len - 1 || last garden /= 2^len - 1
                || garden !! startY /= 2^len - 1
                || 0 < length [() | row <- garden,
                                    not (testBit row startX)
                                        || not (testBit row 0)
                                        || not (testBit row (len-1))] =
        error "input does not fit assumptions"
  | otherwise = (garden,garden2h,garden2w,garden4x,start,len)
  where
    (garden,start@(startX,startY),len) = parse input
    garden2h = garden++garden
    garden4x = garden2w++garden2w
    garden2w = [row .|. shiftL row len | row <- garden]

reachCount :: [Integer] -> [Integer] -> [(Int,Int)]
reachCount garden start = collect $ zip [0..] $ iterate (step garden) start
  where
    collect (_:(t,a):rest@(_:(_,b):_))
      | a == b = [(t,sum (map popCount a))]
      | otherwise = (t,sum (map popCount a)) : collect rest

centerCount :: [Integer] -> [Integer] -> Int
centerCount garden = snd . last . reachCount garden

stripCount :: [Integer] -> Int -> [Integer] -> Int -> Int
stripCount garden len start nsteps =
    fullyReachable*fullCount
        + sum [reach | n <- [0..reachable-fullyReachable-1],
                       t <- [nsteps - (n+fullyReachable)*len],
                       Just reach <- [lookup t counts]]
  where
    counts = reachCount garden start
    (fullSteps,fullCount) = last counts
    reachable = nsteps `div` len + 1
    fullyReachable = (nsteps - fullSteps) `div` len + 1

cornerCount :: [Integer] -> Int -> [Integer] -> Int -> Int
cornerCount garden len start nsteps =
    fullyReachable*(fullyReachable+1)*fullCount `div` 2
        + sum [reach*(fullyReachable+1+n)
               | n <- [0..reachable-fullyReachable-1],
                 t <- [nsteps - (n+fullyReachable)*len],
                 Just reach <- [lookup t counts]]
  where
    counts = reachCount garden start
    (fullSteps,fullCount) = last counts
    reachable = nsteps `div` len + 1
    fullyReachable = (nsteps - fullSteps) `div` len + 1

result2 nsteps ncpu
        (g,g2h,g2w,g4x,start@(startX,startY),len) =
    parallelMapReduce ncpu id sum
        [centerCount g (startBits start len),
         stripCount  g2h (2*len) (startBits (startX,0)        (2*len)) ns,
         stripCount  g2h (2*len) (startBits (startX,2*len-1)  (2*len)) ns,
         stripCount  g2w (2*len) (startBits (0,      startY)  len)     ns,
         stripCount  g2w (2*len) (startBits (2*len-1,startY)  len)     ns,
         cornerCount g4x (2*len) (startBits (0,      0)       (2*len)) nc,
         cornerCount g4x (2*len) (startBits (0,      2*len-1) (2*len)) nc,
         cornerCount g4x (2*len) (startBits (2*len-1,0)       (2*len)) nc,
         cornerCount g4x (2*len) (startBits (2*len-1,2*len-1) (2*len)) nc]
  where
    ns = nsteps - (startX+1)
    nc = nsteps - 2*(startX+1)

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
