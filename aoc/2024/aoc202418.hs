module AOC202418 where

import Data.Map(empty,insert,(!))
import qualified Data.Map
import Data.Set(fromList,member)

import AOC

aoc = AOC {
    day="18",
    testData=unlines [
    "5,4",
    "4,2",
    "4,5",
    "3,0",
    "2,1",
    "6,3",
    "2,4",
    "1,5",
    "0,6",
    "3,3",
    "2,6",
    "5,1",
    "1,2",
    "5,5",
    "2,5",
    "6,5",
    "1,4",
    "0,4",
    "6,4",
    "1,1",
    "6,1",
    "1,0",
    "0,5",
    "1,6",
    "2,0"
    ],
    testResult="22",
    testData2="",
    testResult2="(6,1)",
    aocParse=parse,
    aocTest=result 6 12,
    aocResult=result 70 1024,
    aocParse2=parse,
    aocTest2=result2 6,
    aocResult2=result2 70
    }

parse = toXY . parseInts
  where
    toXY [] = []
    toXY (x:y:rest) = (x,y):toXY rest

result xymax byteCount input = steps
  where
    Just (steps,_) = astar h neighbors snd done [(0,(0,0))]
    space = fromList $ take byteCount input
    h (n,(x,y)) = n+xymax-x+xymax-y
    neighbors (n,(x,y)) =
        [(n+1,(x+dx,y+dy))
         | (dx,dy) <- [(0,1),(1,0),(-1,0),(0,-1)],
           x+dx >= 0, x+dx <= xymax, y+dy >= 0, y+dy <= xymax,
           not (member (x+dx,y+dy) space)]
    done (_,(x,y)) = (x,y) == (xymax,xymax)

data Corruption = BottomLeft | TopRight | Island deriving Eq

result2 xymax input = add empty input
  where
    add grid (xy@(x,y):rest) =
        join xyCorruption []
             [(nxy,grid!nxy) | nxy <- neighbors xy, Data.Map.member nxy grid]
      where
        xyCorruption
          | x == 0 || y == xymax = BottomLeft
          | x == xymax || y == 0 = TopRight
          | otherwise = Island

        join Island _ [] = add (insert xy Island grid) rest
        join corruption islands [] =
            fill corruption (insert xy corruption grid) islands
        join corruption islands ((nxy,Island):ns) =
            join corruption (nxy:islands) ns
        join Island islands ((_,corruption):ns) = join corruption islands ns
        join corruption1 islands ((_,corruption2):ns)
          | corruption1 == corruption2 = join corruption1 islands ns
          | otherwise = xy

        fill corruption g [] = add g rest
        fill corruption g (nxy:queue)
          | maybe True (== corruption) (Data.Map.lookup nxy g) =
              fill corruption g queue
          | otherwise =
              fill corruption (insert nxy corruption g)
                              (neighbors nxy ++ queue)
    neighbors (x,y) =
        [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]
