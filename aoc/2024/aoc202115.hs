module AOC202115 where

import Data.Array(Array,array,assocs,bounds,(!))
import Data.Char(ord)

import AOC

aoc = AOC {
    day="../../2021/input/15",
    aocTests=[
        AOCTest {
            testData=unlines [
                "1163751742",
                "1381373672",
                "2136511328",
                "3694931569",
                "7463417111",
                "1319128137",
                "1359912421",
                "3125421639",
                "1293138521",
                "2311944581"
                ],
            testResult=Just "40",
            testResult2=Just "315"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result,
        codeResult=result,
        codeResult2=result
        }
    }

parse = p . parse2da
  where
    p grid = array (bounds grid) $ map f $ assocs grid
    f (xy,ch) = (xy,ord ch - ord '0')

parse2 = p . parse2da
  where
    p grid = array ((0,0),(5*xmax+4,5*ymax+4)) $ concatMap f $ assocs grid
      where
        ((0,0),(xmax,ymax)) = bounds grid
        f ((x,y),ch) = [((x+dx*(xmax+1),y+dy*(ymax+1)),
                          (ord ch - ord '1' + dx + dy) `mod` 9 + 1)
                        | dx <- [0..4], dy <- [0..4]]

result grid = finalRisk
  where
    ((0,0),(xmax,ymax)) = bounds grid
    Just (finalRisk,_) = astar heuristic neighbors toState done [(0,(0,0))]

    heuristic :: (Int,(Int,Int)) -> Int
    heuristic (cost,(x,y)) = cost + abs (xmax-x) + abs (ymax-y)

    neighbors :: (Int,(Int,Int)) -> [(Int,(Int,Int))]
    neighbors (cost,(x,y)) =
        [(cost+grid!(x+dx,y+dy),(x+dx,y+dy))
         | (dx,dy) <- [(1,0),(0,1),(-1,0),(0,-1)],
           x+dx >= 0, x+dx <= xmax, y+dy >= 0, y+dy <= ymax]

    toState :: (Int,(Int,Int)) -> (Int,Int)
    toState = snd

    done :: (Int,(Int,Int)) -> Bool
    done (_,xy) = xy == (xmax,ymax)
