module AOC201518 where

import Data.Array(Array,bounds,(!))
import Data.Bits(popCount,setBit,shiftL,(.&.),(.|.))
import Data.List(group,sort,subsequences)

import AOC

aoc = AOC {
    day="../../2015/input/18",
    aocTests=[
        AOCTest {
            testData=unlines [
                ".#.#.#",
                "...##.",
                "#....#",
                "..#...",
                "#.#..#",
                "####.."
                ],
            testResult=Just "4",
            testResult2=Just "17"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 6 4,
        codeTest2=result2 6 5,
        codeResult=result 100 100,
        codeResult2=result2 100 100
        }
    }

toBits :: Array (Int,Int) Char -> [Integer]
toBits grid =
    [sum [2^(x+1) | x <- [0..xmax], grid!(x,y) == '#'] | y <- [0..ymax]]
  where
    ((0,0),(xmax,ymax)) = bounds grid

parse :: String -> [Integer]
parse = toBits . parse2da

neighborCounts :: Int -> [Integer] -> [[Int]]
neighborCounts size grid = zipWith3 sum3 byRow
                                         (repeat 0:byRow)
                                         (drop 1 byRow ++ [repeat 0])
  where
    byRow = map rowNeighbors grid
    rowNeighbors row = [popCount (row .&. (shiftL 7 i)) | i <- [0..size-1]]
    sum3 a b c = zipWith3 add3 a b c
    add3 a b c = a + b + c

life :: Int -> [Integer] -> [Integer]
life size grid = zipWith lifeRow (neighborCounts size grid) grid
  where
    lifeRow neighbors row = (row .&. fours) .|. threes
      where
        cells = zip [2^i | i <- [1..]] neighbors
        fours = sum $ map fst $ filter ((== 4) . snd) cells
        threes = sum $ map fst $ filter ((== 3) . snd) cells

result size n = sum . map popCount . head . drop n . iterate (life size)

addCorners :: Int -> [Integer] -> [Integer]
addCorners size (row:rows) = setBit (setBit row 1) size : addBottom rows
  where
    addBottom [row] = [setBit (setBit row 1) size]
    addBottom (row:rows) = row:addBottom rows

result2 size n = sum . map popCount . addCorners size
                     . head . drop n . iterate (life size . addCorners size)
