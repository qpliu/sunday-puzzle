module AOC202504 where

import Data.Map(delete,keys,member,size)
import qualified Data.Map

import AOC

aoc = AOC {
    day="04",
    aocTests=[
        AOCTest {
            testData=unlines [
                "..@@.@@@@.",
                "@@@.@.@.@@",
                "@@@@@.@.@@",
                "@.@@@@..@.",
                "@@.@@@@.@@",
                ".@@@@@@@.@",
                ".@.@.@.@@@",
                "@.@@@.@@@@",
                ".@@@@@@@@.",
                "@.@.@@@.@."
            ],
            testResult=Just "13",
            testResult2=Just "43"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2 0,
        codeResult=result,
        codeResult2=result2 0
        }
    }

parse = Data.Map.filter (== '@') . parse2d

moveable grid = filter notBlocked $ keys grid
  where
    notBlocked (x,y) =
        4 > length [() | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0),
                         member (x+dx,y+dy) grid]

result = length . moveable

result2 n grid
  | null removed = n
  | otherwise = result2 (n + length removed) (foldr delete grid removed)
  where removed = moveable grid

-- This is kind of slow, part 1 is about 0.18s and part 2 is about 2.91s.
-- The only idea I have to go faster is to use an Integer as bit set for
-- each row and somehow use bitwise ops to select the moveable rolls.
