module AOC201613 where

import Data.Bits(popCount)
import Data.Set(Set,difference,elems,empty,fromList,member,
                partition,size,union)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2016/input/13",
    aocTests=[
        AOCTest {
            testData="10",
            testResult=Just "11",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result (7,4),
        codeTest2=result2,
        codeResult=result (31,39),
        codeResult2=result2
        }
    }

parse = head . parseInts

type XY = (Int,Int)

openSpace :: Int -> XY -> Bool
openSpace number (x,y) =
  x >= 0 && y >= 0 && (even $ popCount $ number + x*x + 3*x + 2*x*y + y + y*y)

search :: Int -> Set XY -> Set XY -> XY -> (XY -> Bool) -> Int
search nsteps seen current target open
  | member target current = nsteps
  | otherwise = search (nsteps+1) nextSeen next target open
  where
    nextSeen = union seen current
    next = fromList [xy | (x,y) <- elems current,
                          xy <- [(x-1,y),(x,y-1),(x+1,y),(x,y+1)],
                          not $ member xy nextSeen, open xy]

result target = search 0 empty (fromList [(1,1)]) target . openSpace

walk :: Int -> Set XY -> Set XY -> (XY -> Bool) -> Set XY
walk nsteps reached current open
  | nsteps <= 0 = nextReached
  | otherwise = walk (nsteps-1) nextReached next open
  where
    nextReached = union current reached
    next = fromList [xy | (x,y) <- elems current,
                          xy <- [(x-1,y),(x,y-1),(x+1,y),(x,y+1)],
                          not $ member xy nextReached, open xy]

result2 = size . walk 50 empty (fromList [(1,1)]) . openSpace
