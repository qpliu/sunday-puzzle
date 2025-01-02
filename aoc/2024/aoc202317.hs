module AOC202317 where

import Data.Char(ord)
import Data.Array(bounds,inRange,(!))

import AOC

aoc = AOC {
    day="../../2023/input/17",
    aocTests=[
        AOCTest {
            testData=unlines [
                "2413432311323",
                "3215453535623",
                "3255245654254",
                "3446585845452",
                "4546657867536",
                "1438598798454",
                "4457876987766",
                "3637877979653",
                "4654967986887",
                "4564679986453",
                "1224686865563",
                "2546548887735",
                "4322674655533"
                ],
            testResult=Just "102",
            testResult2=Just "94"
            },
        AOCTest {
            testData=unlines [
                "111111111111",
                "999999999991",
                "999999999991",
                "999999999991",
                "999999999991"
                ],
            testResult=Nothing,
            testResult2=Just "71"
            }
        ],
    aocCode=Code {
        codeParse=parse2da,
        codeParse2=parse2da,
        codeTest=result 1 3,
        codeTest2=result 4 10,
        codeResult=result 1 3,
        codeResult2=result 4 10
        }
    }

result minMoves maxMoves grid = heuristic path
  where
    Just path = astar heuristic neighbors toState done initialPaths
    loss xy = ord (grid!xy) - ord '0'
    (start@(xmin,ymin),end@(xmax,ymax)) = bounds grid
    inBounds = inRange (bounds grid)
    heuristic (cost,xy@(x,y),(dx,dy)) = cost + xmax-x + ymax-y
    neighbors (cost,xy@(x,y),(dx,dy)) =
        [(cost+sum [loss (x-i*dy,y+i*dx) | i <- [1..n]],
          (x-n*dy,y+n*dx),(-dy,dx)) | n <- [minMoves..maxMoves],
                                      inBounds (x-n*dy,y+n*dx)]
        ++ [(cost+sum [loss (x+i*dy,y-i*dx) | i <- [1..n]],
            (x+n*dy,y-n*dx),(dy,-dx)) | n <- [minMoves..maxMoves],
                                        inBounds (x+n*dy,y-n*dx)]
    toState (cost,xy,dxy) = (xy,dxy)
    done (_,xy,_) = xy == end
    initialPaths = [(0,start,(0,1)),(0,start,(1,0))]
