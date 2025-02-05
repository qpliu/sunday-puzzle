module AOC201622 where

import Data.Map(Map,fromList,keys,toList,member)
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2016/input/22",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Filesystem            Size  Used  Avail  Use%",
                "/dev/grid/node-x0-y0   10T    8T     2T   80%",
                "/dev/grid/node-x0-y1   11T    6T     5T   54%",
                "/dev/grid/node-x0-y2   32T   28T     4T   87%",
                "/dev/grid/node-x1-y0    9T    7T     2T   77%",
                "/dev/grid/node-x1-y1    8T    0T     8T    0%",
                "/dev/grid/node-x1-y2   11T    7T     4T   63%",
                "/dev/grid/node-x2-y0   10T    6T     4T   60%",
                "/dev/grid/node-x2-y1    9T    8T     1T   88%",
                "/dev/grid/node-x2-y2    9T    6T     3T   66%"
                ],
            testResult=Nothing,
            testResult2=Just "7"
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

parse = fromList . concatMap (toNode . parseInts) . lines
  where
    toNode [x,y,_,used,avail,_] = [((x,y),(used,avail))]
    toNode _ = []

result nodes = length [() | (xy1,(used1,avail1)) <- toList nodes,
                            (xy2,(used2,avail2)) <- toList nodes,
                            xy1 > xy2,
                            (0 < used1 && used1 <= avail2)
                                || (0 < used2 && used2 <= avail1)]

-- My input data has one empty node and no other node has enough space
-- for the data in any other node.  There are also a few nodes that have
-- too much for the empty node.

result2 allNodes = cost
  where
    [(zero,(0,avail0))] = toList $ Data.Map.filter ((== 0) . fst) allNodes
    nodes = Data.Map.filter ((<= avail0) . fst) allNodes
    goal = maximum $ filter ((== 0) . snd) $ keys nodes

    Just (cost,_) = astar heuristic neighbors toState done initialPaths

    initialPaths = [(0,(goal,zero))]
    done (_,(g,z)) = g == (0,0)
    toState = snd

    dist (gx,gy) (zx,zy) = abs (gx-zx) + abs (gy-zy)

    heuristic (cost,(gxy@(gx,gy),zxy)) =
        cost + 2*(gx + gy) + max 0 (dist gxy zxy - 1)

    neighbors (cost,(gxy,zxy@(x,y))) =
        (if dist gxy zxy == 1
           then ((cost+1,(zxy,gxy)):)
           else id)
            [(cost+1,(gxy,xy)) | xy <- [(x-1,y),(x,y-1),(x+1,y),(x,y+1)],
                                 xy /= gxy, member xy nodes]
