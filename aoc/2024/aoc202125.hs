module AOC202125 where

import Data.Array(assocs,bounds)
import Data.Set(Set,fromList,member)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2021/input/25",
    aocTests=[
        AOCTest {
            testData=unlines [
                "v...>>.vv>",
                ".vv>>.vv..",
                ">>.>v>...v",
                ">>v>>.>.v.",
                "v>v.vv.v..",
                ">.>>..v...",
                ".vv..>.>v.",
                "v.v..>>v.v",
                "....v..v.>"
                ],
            testResult=Just "58",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=const (),
        codeResult=result,
        codeResult2=const ()
        }
    }

parse input = (xymax,fromList east,fromList south)
  where
    grid = parse2da input
    (_,xymax) = bounds grid
    east = map fst $ filter ((== '>') . snd) $ assocs grid
    south = map fst $ filter ((== 'v') . snd) $ assocs grid

step :: (Int,Int) -> (Set (Int,Int),Set (Int,Int))
                  -> (Set (Int,Int),Set (Int,Int))
step (xmax,ymax) (east,south) = (nextEast,nextSouth)
  where
    nextEast = Data.Set.map moveEast east
    nextSouth = Data.Set.map moveSouth south
    moveEast xy@(x,y) | member nextXY east || member nextXY south = xy
                      | otherwise = nextXY
      where nextXY = ((x+1) `mod` (xmax+1),y)
    moveSouth xy@(x,y) | member nextXY south || member nextXY nextEast = xy
                       | otherwise = nextXY
      where nextXY = (x,(y+1) `mod` (ymax+1))

find :: Eq a => [(Int,a)] -> Int
find ((_,a1):rest@((i,a2):_)) | a1 == a2 = i | otherwise = find rest

result (xymax,east,south) =
    find $ zip [0..] $ iterate (step xymax) (east,south)
