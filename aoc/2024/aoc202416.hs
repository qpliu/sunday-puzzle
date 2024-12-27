module AOC202416 where

import Data.Array(Array,assocs,(!))
import qualified Data.Array
import Data.Map(Map,empty,insert,mapWithKey,member,unionWith)
import qualified Data.Map
import Data.Maybe(catMaybes)
import Data.Set(Set,fromList,size,toList,union,unions)
import qualified Data.Set

import AOC

aoc = AOC {
    day="16",
    aocTests=[
        AOCTest {
            testData=unlines [
                "###############",
                "#.......#....E#",
                "#.#.###.#.###.#",
                "#.....#.#...#.#",
                "#.###.#####.#.#",
                "#.#.#.......#.#",
                "#.#.#####.###.#",
                "#...........#.#",
                "###.#.#####.#.#",
                "#...#.....#.#.#",
                "#.#.#.###.#.#.#",
                "#.....#...#.#.#",
                "#.###.#.#.#.#.#",
                "#S..#.....#...#",
                "###############"
                ],
            testResult=Just "7036",
            testResult2=Just "45"
            },
        AOCTest {
            testData=unlines [
                "#################",
                "#...#...#...#..E#",
                "#.#.#.#.#.#.#.#.#",
                "#.#.#.#...#...#.#",
                "#.#.#.#.###.#.#.#",
                "#...#.#.#.....#.#",
                "#.#.#.#.#.#####.#",
                "#.#...#.#.#.....#",
                "#.#.#####.#.###.#",
                "#.#.#.......#...#",
                "#.#.###.#####.###",
                "#.#.#...#.....#.#",
                "#.#.#.#####.###.#",
                "#.#.#.........#.#",
                "#.#.#.#########.#",
                "#S#.............#",
                "#################"
                ],
            testResult=Just "11048",
            testResult2=Just "64"
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

parse input = (getStart grid,getEnd grid,toGraph grid)
  where grid = parse2da input

getStart = fst . head . filter ((== 'S') . snd) . assocs

getEnd = fst . head . filter ((== 'E') . snd) . assocs

turnL (dx,dy) = (dy,-dx)

turnR (dx,dy) = (-dy,dx)

turnU (dx,dy) = (-dx,-dy)

(<<) (x,y) (dx,dy) = (x+dx,y+dy)

type XY = (Int,Int)

type DXY = (Int,Int)

toGraph :: Array XY Char -> Map (XY,DXY) (Int,(XY,DXY),Set XY)
toGraph grid = dfs empty (empty,[start])
  where
    start = getStart grid
    end = getEnd grid
    dfs :: Map XY () -> (Map (XY,DXY) (Int,(XY,DXY),Set XY),[XY]) -> Map (XY,DXY) (Int,(XY,DXY),Set XY)
    dfs seen (graph,[]) = graph
    dfs seen (graph,(xy0:queue))
      | member xy0 seen = dfs seen (graph,queue)
      | otherwise =
          dfs (insert xy0 () seen) $ foldr walk (graph,queue)
                                           [(1,0),(-1,0),(0,1),(0,-1)]
      where
        walk :: DXY -> (Map (XY,DXY) (Int,(XY,DXY),Set XY),[XY]) -> (Map (XY,DXY) (Int,(XY,DXY),Set XY),[XY])
        walk dxy0 gq@(g,q)
          | grid Data.Array.!(xy0<<dxy0) == '#' = gq
          | otherwise = walk1 (xy0<<dxy0) dxy0 [xy0] 1
          where
            walk1 :: XY -> DXY -> [XY] -> Int -> (Map (XY,DXY) (Int,(XY,DXY),Set XY),[XY])
            walk1 xy dxy tiles cost
              | xy == end = (insert (xy0,dxy0) (cost,(xy,dxy),fromList tiles) g,q)
              | exits == (False,False,False) = gq -- dead end
              | exits == (True,False,False) = walk1 (xy<<dxy) dxy (xy:tiles) (cost+1)
              | exits == (False,True,False) =
                  walk1 (xy<<turnL dxy) (turnL dxy) (xy:tiles) (cost+1001)
              | exits == (False,False,True) =
                  walk1 (xy<<turnR dxy) (turnR dxy) (xy:tiles) (cost+1001)
              | otherwise = (insert (xy0,dxy0) (cost,(xy,dxy),fromList tiles) g,xy:q)
              where
                exits = (grid Data.Array.!(xy<<dxy) /= '#',
                         grid Data.Array.!(xy<<turnL dxy) /= '#',
                         grid Data.Array.!(xy<<turnR dxy) /= '#')

type State = (XY,DXY)
type Path = (Int,(State,Set XY))

heuristic :: XY -> Path -> Int
heuristic end@(endX,endY) (score,((xy@(x,y),dxy@(dx,dy)),_))
  | xy == end = score
  | signum (endX-x) == dx && y == endY = score + abs (endX-x)
  | signum (endX-x) == dx = score + abs (endX-x) + abs (endY-y) + 1000
  | signum (endY-y) == dy && x == endX = score + abs (endY-y)
  | signum (endY-y) == dy = score + abs (endX-x) + abs (endY-y) + 1000
  | otherwise = score + abs (endX-x) + abs (endY-y) + 2000

neighbors :: Map (XY,DXY) (Int,(XY,DXY),Set XY) -> Path -> [Path]
neighbors graph (score,((srcXY,srcDxy),_)) =
    [(score+turnScore+cost,(dest,tiles))
     | (turnScore,src) <- [(0,(srcXY,srcDxy)),
                           (1000,(srcXY,turnL srcDxy)),
                           (1000,(srcXY,turnR srcDxy))],
       member src graph,
       (cost,dest,tiles) <- [graph Data.Map.! src]]

toState :: Path -> State
toState (_,(xyDxy,_)) = xyDxy

done :: XY -> Path -> Bool
done end (_,((xy,_),_)) = xy == end

initialPaths :: XY -> [Path]
initialPaths start = [(0,((start,(1,0)),Data.Set.empty))]

result (start,end@(endX,endY),graph) = bestScore
  where
    Just (bestScore,_) =
        astar (heuristic end) (neighbors graph) toState (done end)
              (initialPaths start)

result2 (start,end@(endX,endY),graph) =
    size bestXYs + 1 -- end tile not included in the search
  where
    Just bestXYs =
        astarAll (heuristic end) (neighbors graph) toState (done end)
                 makeBest mergeBest (initialPaths start)

    makeBest = unions . map getTiles
    mergeBest best = unions . (best:) . map getTiles
    getTiles (_,(_,tiles)) = tiles
