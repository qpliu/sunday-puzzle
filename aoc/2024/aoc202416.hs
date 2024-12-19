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
    testResult="11048",
    testData2="",
    testResult2="64",
    aocParse=parse,
    aocTest=result,
    aocResult=result,
    aocParse2=parse2,
    aocTest2=result2,
    aocResult2=result2
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

toGraph :: Array XY Char -> Map (XY,DXY) (Int,(XY,DXY))
toGraph grid = dfs empty (empty,[start])
  where
    start = getStart grid
    end = getEnd grid
    dfs :: Map XY () -> (Map (XY,DXY) (Int,(XY,DXY)),[XY]) -> Map (XY,DXY) (Int,(XY,DXY))
    dfs seen (graph,[]) = graph
    dfs seen (graph,(xy0:queue))
      | member xy0 seen = dfs seen (graph,queue)
      | otherwise =
          dfs (insert xy0 () seen) $ foldr walk (graph,queue)
                                           [(1,0),(-1,0),(0,1),(0,-1)]
      where
        walk :: DXY -> (Map (XY,DXY) (Int,(XY,DXY)),[XY]) -> (Map (XY,DXY) (Int,(XY,DXY)),[XY])
        walk dxy0 gq@(g,q)
          | grid Data.Array.!(xy0<<dxy0) == '#' = gq
          | otherwise = walk1 (xy0<<dxy0) dxy0 1
          where
            walk1 :: XY -> DXY -> Int -> (Map (XY,DXY) (Int,(XY,DXY)),[XY])
            walk1 xy dxy cost
              | xy == end = (insert (xy0,dxy0) (cost,(xy,dxy)) g,q)
              | exits == (False,False,False) = gq -- dead end
              | exits == (True,False,False) = walk1 (xy<<dxy) dxy (cost+1)
              | exits == (False,True,False) =
                  walk1 (xy<<turnL dxy) (turnL dxy) (cost+1001)
              | exits == (False,False,True) =
                  walk1 (xy<<turnR dxy) (turnR dxy) (cost+1001)
              | otherwise = (insert (xy0,dxy0) (cost,(xy,dxy)) g,xy:q)
              where
                exits = (grid Data.Array.!(xy<<dxy) /= '#',
                         grid Data.Array.!(xy<<turnL dxy) /= '#',
                         grid Data.Array.!(xy<<turnR dxy) /= '#')

type Path1 = (Int,(XY,DXY))

result (start,end@(endX,endY),graph) = bestScore
  where
    Just (bestScore,_) = astar h neighbors snd done [(0,(start,(1,0)))]
    h (score,(xy@(x,y),dxy@(dx,dy)))
      | xy == end = score
      | signum (endX-x) == dx && y == endY = score + abs (endX-x)
      | signum (endX-x) == dx = score + abs (endX-x) + abs (endY-y) + 1000
      | signum (endY-y) == dy && x == endX = score + abs (endY-y)
      | signum (endY-y) == dy = score + abs (endX-x) + abs (endY-y) + 1000
      | otherwise = score + abs (endX-x) + abs (endY-y) + 2000
    neighbors (score,(srcXY,srcDxy)) =
        [(score+turnScore+cost,dest)
         | (turnScore,src) <- [(0,(srcXY,srcDxy)),
                               (1000,(srcXY,turnL srcDxy)),
                               (1000,(srcXY,turnR srcDxy))],
           member src graph,
           (cost,dest) <- [graph Data.Map.! src]]
    done (_,(xy,_)) = xy == end

parse2 :: String -> (XY,XY,Map (XY,DXY) (Int,(XY,DXY)),(XY,DXY) -> Set XY)
parse2 input = (getStart grid,getEnd grid,graph,getTiles grid graph)
  where
    grid = parse2da input
    graph = toGraph grid

getTiles :: Array XY Char -> Map (XY,DXY) (Int,(XY,DXY)) -> (XY,DXY) -> Set XY
getTiles grid graph = (tiles Data.Map.!)
  where
    tiles = mapWithKey toTiles graph
    toTiles xyDxy0 (_,xyDxy1) = fromList $ walk xyDxy0
      where
        walk xyDxy@(xy,dxy)
          | xyDxy == xyDxy1 = []
          | grid Data.Array.! (xy<<dxy) /= '#' = xy : walk (xy<<dxy,dxy)
          | grid Data.Array.! (xy<<turnL dxy) /= '#' = xy : walk (xy<<turnL dxy,turnL dxy)
          | otherwise = xy : walk (xy<<turnR dxy,turnR dxy)

type Path2 = (Int,((XY,DXY),Maybe (XY,DXY)))

result2 :: (XY,XY,Map (XY,DXY) (Int,(XY,DXY)),(XY,DXY) -> Set XY) -> Int
result2 (start,end@(endX,endY),graph,tiles) =
    size bestXYs + 1 -- end tile not included in the search
  where
    Just bestXYs = astarAll h neighbors toState done makeBest mergeBest
                            [(0,((start,(1,0)),Nothing))]
    h (score,((xy@(x,y),dxy@(dx,dy)),_))
      | xy == end = score
      | signum (endX-x) == dx && y == endY = score + abs (endX-x)
      | signum (endX-x) == dx = score + abs (endX-x) + abs (endY-y) + 1000
      | signum (endY-y) == dy && x == endX = score + abs (endY-y)
      | signum (endY-y) == dy = score + abs (endX-x) + abs (endY-y) + 1000
      | otherwise = score + abs (endX-x) + abs (endY-y) + 2000
    neighbors (score,((srcXY,srcDxy),_)) =
        [(score+turnScore+cost,(dest,Just src))
         | (turnScore,src) <- [(0,(srcXY,srcDxy)),
                               (1000,(srcXY,turnL srcDxy)),
                               (1000,(srcXY,turnR srcDxy))],
           member src graph,
           (cost,dest) <- [graph Data.Map.! src]]
    toState (_,(xyDxy,_)) = xyDxy
    done (_,((xy,_),_)) = xy == end

    makeBest :: [Path2] -> Set XY
    makeBest = Data.Set.unions . map getTiles

    mergeBest :: Set XY -> [Path2] -> Set XY
    mergeBest best = Data.Set.unions . (best:) . map getTiles

    getTiles :: Path2 -> Set XY
    getTiles (_,(_,Just src)) = tiles src
    getTiles _ = Data.Set.empty
