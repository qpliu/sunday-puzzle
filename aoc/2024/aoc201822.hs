module AOC201822 where

import Data.Map(Map,fromList,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2018/input/22",
    aocTests=[
        AOCTest {
            testData="510 10,10",
            testResult=Just "114",
            testResult2=Just "45"
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=parseInts,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

type XY = (Int,Int)

makeCave :: Bool -> [Int] -> (XY,Map XY Int)
makeCave expand [depth,targetX,targetY] =
    ((targetX,targetY),Data.Map.map (`mod` 3) table)
  where
    table =
        fromList [((x,y),erosionLevel x y) | x <- [0..maxX], y <- [0..maxY]]
    erosionLevel x y
      | (x,y) == (targetX,targetY) = depth `mod` 20183
      | x == 0 = (y*48271 + depth) `mod` 20183
      | y == 0 = (x*16807 + depth) `mod` 20183
      | otherwise = (table!(x-1,y)*table!(x,y-1) + depth) `mod` 20183
    (maxX,maxY)
      | expand = (max targetX targetY*2,max targetX targetY*2)
      | otherwise = (targetX,targetY)

result = sum . snd . makeCave False

result2 input = cost
  where
    (targetXY@(targetX,targetY),cave) = makeCave True input
    Just (cost,_) = astar heuristic neighbors toState done initialPaths

    initialPaths = [(0,((0,0),1))]
    done (_,(xy,tool)) = xy == targetXY && tool == 1
    toState = snd

    heuristic (cost,((x,y),tool)) =
        cost + abs (x-targetX) + abs (y-targetY) + if tool == 1 then 0 else 7

    neighbors (cost,(xy@(x,y),tool)) =
        [(cost+7,(xy,newTool)) | newTool <- [0..2],
                                 newTool /= tool,
                                 newTool /= cave!xy] ++
        [(cost+1,(newXY,tool))
         | newXY@(newX,newY) <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)],
           newX >= 0, newY >= 0, tool /= cave!newXY]
