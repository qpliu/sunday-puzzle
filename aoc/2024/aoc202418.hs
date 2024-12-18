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

result xymax byteCount input = fst $ astar h neighbors snd done [(0,(0,0))]
  where
    space = fromList $ take byteCount input
    h (n,(x,y)) = n+xymax-x+xymax-y
    neighbors (n,(x,y)) =
        [(n+1,(x+dx,y+dy))
         | (dx,dy) <- [(0,1),(1,0),(-1,0),(0,-1)],
           x+dx >= 0, x+dx <= xymax, y+dy >= 0, y+dy <= xymax,
           not (member (x+dx,y+dy) space)]
    done (_,(x,y)) = (x,y) == (xymax,xymax)

result2 xymax input = add empty input
  where
    -- (hits top,hits left,hits right,hits bottom)
    add grid (xy@(x,y):rest)
      | (stp && slt) || (srt && sbt) || (srt && slt) || (stp && sbt) = xy
      | otherwise = add (fill state (insert xy state grid) (neighbors xy)) rest
      where
        state@(stp,slt,srt,sbt) =
            foldr join (y == 0,x == 0,x == xymax,y == xymax) (neighbors xy)
        join nxy st@(tp,lt,rt,bt) = maybe st j $ Data.Map.lookup nxy grid
          where j (tp2,lt2,rt2,bt2) = (tp || tp2,lt || lt2,rt || rt2,bt || bt2)
    neighbors (x,y) =
        [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]
    fill state g [] = g
    fill state g (xy:queue)
      | not (Data.Map.member xy g) || g!xy == state = fill state g queue
      | otherwise = fill state (insert xy state g) ((neighbors xy) ++ queue)
