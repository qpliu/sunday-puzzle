module AOC202404 where

import Data.Map(toList)
import qualified Data.Map

import AOC

aoc = AOC {
    day="04",
    testData=unlines [
    "MMMSXXMASM",
    "MSAMXMSMSA",
    "AMXSXMAAMM",
    "MSAMASMSMX",
    "XMASAMXAMM",
    "XXAMMXXAMA",
    "SMSMSASXSS",
    "SAXAMASAAA",
    "MAMMMXMMMM",
    "MXMXAXMASX"
    ],
    testResult="18",
    testData2="",
    testResult2="9",
    aocParse=parse2d,
    aocResult=result,
    aocParse2=parse2d,
    aocResult2=result2
    }

result grid = sum $ map count $ toList grid
  where
    count ((x,y),'X') = sum [c x y dx dy |
                             (dx,dy) <- [(1,0),(1,1),(1,-1),(0,1),(0,-1),
                                         (-1,0),(-1,1),(-1,-1)]]
    count _ = 0
    c x y dx dy
      | [Data.Map.lookup (x+i*dx,y+i*dy) grid | i <- [1..3]] == [Just 'M',Just 'A',Just 'S'] = 1
      | otherwise = 0

result2 grid = sum $ map count $ toList grid
  where
    count ((x,y),'A')
      | [Data.Map.lookup (x+dx,y+dy) grid |
              (dx,dy) <- [(-1,-1),(1,1),(-1,1),(1,-1)]] `elem` [
          [Just 'M',Just 'S',Just 'M',Just 'S'],
          [Just 'M',Just 'S',Just 'S',Just 'M'],
          [Just 'S',Just 'M',Just 'M',Just 'S'],
          [Just 'S',Just 'M',Just 'S',Just 'M']] = 1
      | otherwise = 0
    count _ = 0
