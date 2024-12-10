module AOC202404 where

import Data.Array(Array,bounds,inRange,range,(!))

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
    aocParse=parse2da,
    aocResult=result,
    aocParse2=parse2da,
    aocResult2=result2
    }

result :: Array (Int,Int) Char -> Int
result grid = sum $ map count $ range $ bounds grid
  where
    count (x,y)
      | grid!(x,y) /= 'X' = 0
      | otherwise = sum [mas dxdy |
            dxdy <- [(1,0),(1,1),(1,-1),(0,1),(0,-1),(-1,0),(-1,1),(-1,-1)]]
      where
        mas (dx,dy)
          | not (inRange (bounds grid) (x+3*dx,y+3*dy)) = 0
          | grid!(x+dx,y+dy) /= 'M' = 0
          | grid!(x+2*dx,y+2*dy) /= 'A' = 0
          | grid!(x+3*dx,y+3*dy) /= 'S' = 0
          | otherwise = 1

result2 grid = length $ filter xmas $ range $ bounds grid
  where
    xmas (x,y)
      | grid!(x,y) /= 'A' = False
      | not (inRange (bounds grid) (x+1,y+1)) = False
      | not (inRange (bounds grid) (x-1,y-1)) = False
      | notMS (grid!(x+1,y+1),grid!(x-1,y-1)) = False
      | notMS (grid!(x-1,y+1),grid!(x+1,y-1)) = False
      | otherwise = True
    notMS ('M','S') = False
    notMS ('S','M') = False
    notMS _ = True
