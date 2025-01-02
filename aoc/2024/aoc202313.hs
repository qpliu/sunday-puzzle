module AOC202313 where

import Data.Array(Array,bounds,(!))

import AOC

aoc = AOC {
    day="../../2023/input/13",
    aocTests=[
        AOCTest {
            testData=unlines [
                "#.##..##.",
                "..#.##.#.",
                "##......#",
                "##......#",
                "..#.##.#.",
                "..##..##.",
                "#.#.##.#.",
                "",
                "#...##..#",
                "#....#..#",
                "..##..###",
                "#####.##.",
                "#####.##.",
                "..##..###",
                "#....#..#"
                ],
            testResult=Just "405",
            testResult2=Just "400"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result 0,
        pcodeTest2=result 1,
        pcodeResult=result 0,
        pcodeResult2=result 1
        }
    }

parse :: String -> [Array (Int,Int) Char]
parse = p . span (not . null) . lines
  where
    p (pattern,patterns)
      | null pattern = []
      | otherwise = parse2da (unlines pattern)
                      : p (span (not . null) (dropWhile null patterns))

analyze :: Int -> Array (Int,Int) Char -> Int
analyze smudges pat = head $ verticals ++ horizontals
  where
    ((0,0),(xmax,ymax)) = bounds pat
    isVertical x =
        smudges == length [() | dx <- [0 .. min (x-1) (xmax-x)],
                                y <- [0 .. ymax],
                                pat!(x-1-dx,y) /= pat!(x+dx,y)]
    isHorizontal y =
        smudges == length [() | dy <- [0 .. min (y-1) (ymax-y)],
                                x <- [0 .. xmax],
                                pat!(x,y-1-dy) /= pat!(x,y+dy)]
    verticals = [x | x <- [1..xmax], isVertical x]
    horizontals = [y*100 | y <- [1..ymax], isHorizontal y]

result smudges ncpu = parallelMapReduce ncpu (analyze smudges) sum
