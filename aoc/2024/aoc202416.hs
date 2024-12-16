module AOC202416 where

import Data.Array(Array,assocs)
import qualified Data.Array
import Data.Map(Map,empty,insert,member,(!))
import Data.Set(fromList,size,toList)

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
    aocParse2=parse,
    aocTest2=result2,
    aocResult2=result2
    }

parse = parse2da

start = fst . head . filter ((== 'S') . snd) . assocs

end = fst . head . filter ((== 'E') . snd) . assocs

turnL (dx,dy) = (dy,-dx)

turnR (dx,dy) = (-dy,dx)

toGraph :: Array (Int,Int) Char -> Map (Int,Int) [((Int,Int),(Int,((Int,Int),(Int,Int))))]
toGraph mp = dfs empty [start mp]
  where
    goal = end mp
    dfs g [] = g
    dfs g (xy0@(x0,y0):rest)
      | member xy0 g = dfs g rest
      | otherwise = finishXY $ foldr walk [] [(1,0),(-1,0),(0,1),(0,-1)]
      where
        walk dxy0@(dx0,dy0) dests
          | mp Data.Array.!(x0+dx0,y0+dy0) == '#' = dests
          | otherwise = walk1 1 dxy0 (x0+dx0,y0+dy0)
          where
            walk1 score dxy@(dx,dy) xy@(x,y)
              | xy == goal = (dxy0,(score,(dxy,xy))) : dests
              | length exits == 0 = dests -- dead end
              | length exits > 1 = (dxy0,(score,(dxy,xy))) : dests
              | exits == [dxy] = walk1 (score+1) dxy (x+dx,y+dy)
              | exits == [turnL dxy] = walk1 (score+1001) (turnL dxy) (x+dy,y-dx)
              | exits == [turnR dxy] = walk1 (score+1001) (turnR dxy) (x-dy,y+dx)
              where
                exits = [dxy1 | dxy1@(dx1,dy1) <- [(1,0),(-1,0),(0,1),(0,-1)],
                                (dx1,dy1) /= (-dx,-dy),
                                mp Data.Array.!(x+dx1,y+dy1) /= '#']
        finishXY nexts = dfs (insert xy0 nexts g) ((map (snd . snd . snd) nexts) ++ rest)

result mp = h $ astar h neighbors getState done [(0,(1,0),start mp)]
  where
    graph = toGraph mp
    getState (_,dxy,xy) = (dxy,xy)
    dest@(destX,destY) = end mp
    h (score,(dx,dy),(x,y))
      | (x,y) == dest = score
      | otherwise =
          score + abs(destX-x) + abs(destY-y)
                + (if signum (destX-x) == dx
                       then 0
                       else (if signum (destX-x) == -dx
                                then 2000
                                else 1000))
                + (if signum (destY-y) == dy
                       then 0
                       else (if signum (destY-y) == -dy
                                then 2000
                                else 1000))
    neighbors (score,dxy@(dx,dy),xy@(x,y)) =
        [(score+dscore+turnScore dxy1,dxy2,xy2) | (dxy1,(dscore,(dxy2,xy2))) <- graph!xy]
      where
        turnScore dxy1
          | dxy1 == dxy = 0
          | dxy1 == turnL dxy || dxy1 == turnR dxy = 1000
          | otherwise = 2000
    done (_,_,xy) = xy == dest

result2 mp = size $ fromList $ concatMap walkTiles
                  $ toList $ fromList $ concatMap getSegs
                  $ astarAll h neighbors getState done [(0,(1,0),start mp,[])]
  where
    graph = toGraph mp
    getState (_,dxy,xy,_) = (dxy,xy)
    dest@(destX,destY) = end mp
    h (score,(dx,dy),(x,y),path)
      | (x,y) == dest = score
      | otherwise =
          score + abs(destX-x) + abs(destY-y)
                + (if signum (destX-x) == dx
                       then 0
                       else (if signum (destX-x) == -dx
                                then 2000
                                else 1000))
                + (if signum (destY-y) == dy
                       then 0
                       else (if signum (destY-y) == -dy
                                then 2000
                                else 1000))
    neighbors (score,dxy@(dx,dy),xy@(x,y),path) =
        [(score+dscore+turnScore dxy1,dxy2,xy2,(xy,dxy1,xy2):path) | (dxy1,(dscore,(dxy2,xy2))) <- graph!xy]
      where
        turnScore dxy1
          | dxy1 == dxy = 0
          | dxy1 == turnL dxy || dxy1 == turnR dxy = 1000
          | otherwise = 2000
    done (_,_,xy,_) = xy == dest
    getSegs (_,_,_,path) = path
    walkTiles (xy0@(x0,y0),dxy0@(dx0,dy0),endXY) =
        xy0 : walk ((x0+dx0,y0+dy0),dxy0)
      where
        walk (xy@(x,y),(dx,dy))
          | xy == endXY = [xy]
          | otherwise = xy : walk next
          where
            [next] = [((x+dx1,y+dy1),dxy1)
                            | dxy1@(dx1,dy1) <- [(1,0),(-1,0),(0,1),(0,-1)],
                              (dx1,dy1) /= (-dx,-dy),
                              mp Data.Array.!(x+dx1,y+dy1) /= '#']
