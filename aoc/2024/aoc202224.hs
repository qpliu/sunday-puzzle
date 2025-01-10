module AOC202224 where

import Data.Array(Array,array,assocs,bounds,(!))
import Data.Set(Set,empty,fromList,insert,member)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2022/input/24",
    aocTests=[
        AOCTest {
            testData=unlines [
                "#.######",
                "#>>.<^<#",
                "#.<..<<#",
                "#>v.><>#",
                "#<^v^^>#",
                "######.#"
                ],
            testResult=Just "18",
            testResult2=Just "54"
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

-- There are no vertical blizzards at the start and goal columns, so
-- the blizzard patterns repeat every 300 minutes (150 columns, 20 rows).
parse input = zip [0..] grids
  where
    (_,(xmax,ymax)) = bounds initialGrid
    initialGrid = parse2da input
    initialBlizzards =
        fromList $ filter ((`elem` "<>v^") . snd) $ assocs initialGrid
    grids = makeGrids empty initialBlizzards

    makeGrids blizzardStates blizzard
      | member blizzard blizzardStates = []
      | otherwise =
              makeGrid blizzard : makeGrids (insert blizzard blizzardStates)
                                            (nextBlizzard blizzard)

    nextBlizzard = Data.Set.map moveBlizzard
    moveBlizzard ((x,y),'<') = (((x-2) `mod` (xmax-1) + 1,y),'<')
    moveBlizzard ((x,y),'>') = ((x `mod` (xmax-1) + 1,y),'>')
    moveBlizzard ((x,y),'^') = ((x,(y-2) `mod` (ymax-1) + 1),'^')
    moveBlizzard ((x,y),'v') = ((x,y `mod` (ymax-1) + 1),'v')

    makeGrid :: Set ((Int,Int),Char) -> Array (Int,Int) Bool
    makeGrid blizzard =
        array (bounds initialGrid) $ map makeGridElement $ assocs initialGrid
      where
        occupied = Data.Set.map fst blizzard
        makeGridElement (xy,ch)
          | ch == '#' || member xy occupied = (xy,False)
          | otherwise = (xy,True)

type XY = (Int,Int)
type Blizzard = (Int,Array XY Bool)

type State = (Int,XY)
type Path = (Int,(State,[Blizzard]))

search :: [Blizzard] -> XY -> XY -> (Int,[Blizzard])
search blizzards@((initialIndex,_):_) startXY@(_,startY) endXY@(endX,endY) =
    toResult finalPath
  where
    ymax = max startY endY

    heuristic (time,((_,(x,y)),_)) = time + abs (x-endX) + abs (y-endY)
    neighbors (time,((_,(x,y)),_:blizzes@((blizIdx,blizGrid):_))) =
        [(time+1,((blizIdx,(newX,newY)),blizzes))
         | newXY@(newX,newY) <- [(x,y),(x+1,y),(x,y+1),(x-1,y),(x,y-1)],
           newY >= 0, newY <= ymax, blizGrid!newXY]
    toState (_,(state,_)) = state
    done (_,((_,xy),_)) = xy == endXY

    initialPaths = [(0,((initialIndex,startXY),blizzards))]
    toResult (time,(_,blizzes)) = (time,blizzes)

    Just finalPath = astar heuristic neighbors toState done initialPaths

result blizzards@((_,grid):_) =
    fst $ search (cycle blizzards) (1,0) (xmax-1,ymax)
  where (_,(xmax,ymax)) = bounds grid

-- Once the fastest time for the first time to the goal is found,
-- there's no point in continuing the search along paths that have yet
-- to reach the goal, since it's always possible to wait at the goal
-- for the best time to start on the way back.  The same reasoning
-- applies after finding the best time back to the start.

-- The best path for the first time from the start to the goal should be
-- saved, so that if the second path from the start to the goal reaches
-- the same state (location and blizzard pattern), it can be used to
-- find how long it takes to reach the end.  But that's an optimization
-- for later.

result2 blizzards@((_,grid):_) = t1 + t2 + t3
  where
    (_,(xmax,ymax)) = bounds grid
    startXY = (1,0)
    endXY = (xmax-1,ymax)
    (t1,blizzards1) = search (cycle blizzards) startXY endXY
    (t2,blizzards2) = search blizzards1 endXY startXY
    (t3,_) = search blizzards2 startXY endXY
