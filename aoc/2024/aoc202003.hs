module AOC202003 where

import Data.Array(bounds,(!))

import AOC

aoc = AOC {
    day="../../2020/input/03",
    aocTests=[
        AOCTest {
            testData=unlines [
                "..##.......",
                "#...#...#..",
                ".#....#..#.",
                "..#.#...#.#",
                ".#...##..#.",
                "..#.##.....",
                ".#.#.#....#",
                ".#........#",
                "#.##...#...",
                "#...##....#",
                ".#..#...#.#"
                ],
            testResult=Just "7",
            testResult2=Just "336"
        }
    ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse = fmap count . parse2da
  where
    count '#' = 1 :: Int
    count '.' = 0

positions :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
positions (xmax,ymax) (dx,dy) =
    [(i*dx `mod` xmax,i*dy) | i <- [0..ymax `div` dy]]

result grid = sum $ map (grid!) $ positions (xmax+1,ymax) (3,1)
  where (_,(xmax,ymax)) = bounds grid

result2 grid = product $ map (sum . map (grid!) . positions (xmax+1,ymax))
                             [(1,1),(3,1),(5,1),(7,1),(1,2)]
  where (_,(xmax,ymax)) = bounds grid
