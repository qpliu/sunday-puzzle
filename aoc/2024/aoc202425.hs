module AOC202425 where

import Data.List(nub)

import AOC

aoc = AOC {
    day="25",
    testData=unlines [
    "#####",
    ".####",
    ".####",
    ".####",
    ".#.#.",
    ".#...",
    ".....",
    "",
    "#####",
    "##.##",
    ".#.##",
    "...##",
    "...#.",
    "...#.",
    ".....",
    "",
    ".....",
    "#....",
    "#....",
    "#...#",
    "#.#.#",
    "#.###",
    "#####",
    "",
    ".....",
    ".....",
    "#.#..",
    "###..",
    "###.#",
    "###.#",
    "#####",
    "",
    ".....",
    ".....",
    ".....",
    "#....",
    "#.#..",
    "#.#.#",
    "#####"
    ],
    testResult="3",
    testData2="",
    testResult2="()",
    aocParse=parse,
    aocTest=result,
    aocResult=result,
    aocParse2=parse,
    aocTest2=const (),
    aocResult2=const ()
    }

parse :: String -> ([[Int]],[[Int]])
parse = p ([],[]) . lines
  where
    p (keys,locks) [] = (keys,locks)
    p (keys,locks) (str:strs)
      | all (== '.') str = pkey (map toKeyHeights str) strs
      | all (== '#') str = plock (map toLockHeights str) strs
      where
        pkey :: [Int] -> [String] -> ([[Int]],[[Int]])
        pkey heights [] = (heights:keys,locks)
        pkey heights ("":rest) = p (heights:keys,locks) rest
        pkey heights (str:rest) =
            pkey (zipWith (+) heights (map toKeyHeights str)) rest
        toKeyHeights '#' = 0
        toKeyHeights '.' = 1
        plock heights [] = (keys,heights:locks)
        plock heights ("":rest) = p (keys,heights:locks) rest
        plock heights (str:rest) =
            plock (zipWith (+) heights (map toLockHeights str)) rest
        toLockHeights '#' = 1
        toLockHeights '.' = 0

fit :: [Int] -> [Int] -> Bool
fit key lock = {- length key == length lock && -} and (zipWith (>=) key lock)

result :: ([[Int]],[[Int]]) -> Int
result (keys,locks) = length [() | k <- keys, l <- locks, fit k l]