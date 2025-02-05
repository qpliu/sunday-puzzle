module AOC201624 where

import Data.Map(Map,toList,(!))
import qualified Data.Map
import Data.Set(Set,delete)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2016/input/24",
    aocTests=[
        AOCTest {
            testData=unlines [
                "###########",
                "#0.1.....2#",
                "#.#######.#",
                "#4.......3#",
                "###########"
                ],
            testResult=Just "14",
            testResult2=Nothing
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

type XY = (Int,Int)

makeGraph :: Map XY Char -> Map XY [(Char,Int,XY)]
makeGraph grid = Data.Map.fromList $ map makeEdges nodeXYs
  where
    nodeXYs = concatMap makeNode $ toList grid
    makeNode (xy@(x,y),ch)
      | ch == '#' = []
      | ch /= '.' = [xy]
      | length neighbors > 2 = [xy]
      | otherwise = []
      where neighbors = [() | nxy <- [(x-1,y),(x,y-1),(x+1,y),(x,y+1)],
                              grid!nxy /= '#']
    makeEdges nodeXY@(nodeX,nodeY) =
        (nodeXY,[edge | dxy@(dx,dy) <- [(-1,0),(0,-1),(1,0),(0,1)],
                        edge <- walk nodeXY (nodeX+dx,nodeY+dy) 1])
    walk lastXY xy@(x,y) steps
      | ch == '#' = []
      | ch /= '.' = [(ch,steps,xy)]
      | null exits = []
      | length exits > 1 = [(ch,steps,xy)]
      | otherwise = walk xy (head exits) (steps+1)
      where
        ch = grid!xy
        exits = [exitXY | exitXY <- [(x-1,y),(x,y-1),(x+1,y),(x,y+1)],
                          exitXY /= lastXY, grid!exitXY /= '#']

parse input = (makeGraph grid,targets,start)
  where
    grid = parse2d input
    [(start,'0')] = filter ((== '0') . snd) $ toList grid
    targets = delete '.' $ delete '#' $ delete '0'
            $ Data.Set.fromList $ map snd $ toList grid

search :: ((Int,(XY,Set Char)) -> Bool)
       -> (Map XY [(Char,Int,XY)],Set Char,XY) -> Int
search done (graph,initialTargets,initialXY) = finalCost
  where
    Just (finalCost,_) =
        astar fst neighbors snd done [(0,(initialXY,initialTargets))]

    neighbors (cost,(xy,targets)) =
        [(cost+steps,(nextXY,delete ch targets))
         | (ch,steps,nextXY) <- graph!xy]

result = search done
  where done (_,(_,targets)) = null targets

result2 input@(_,_,start) = search done input
  where done (_,(xy,targets)) = null targets && xy == start
