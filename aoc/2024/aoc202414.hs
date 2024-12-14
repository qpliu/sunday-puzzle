module AOC202414 where

import Data.Set(fromList,member,toList)

import AOC

aoc = AOC {
    day="14",
    testData=unlines [
    "p=0,4 v=3,-3",
    "p=6,3 v=-1,-3",
    "p=10,3 v=-1,2",
    "p=2,0 v=2,-1",
    "p=0,0 v=1,3",
    "p=3,0 v=-2,-2",
    "p=7,6 v=-1,-3",
    "p=3,0 v=-1,-2",
    "p=9,3 v=2,3",
    "p=7,3 v=-1,2",
    "p=2,4 v=2,-3",
    "p=9,5 v=-3,-3"
    ],
    testResult="12",
    testData2="",
    testResult2="0",
    aocParse=parse . parseInts,
    aocResult=result 101 103 100,
    aocTest=result 11 7 100,
    aocParse2=parse . parseInts,
    aocTest2=const 0,
    aocResult2=result2
    }

parse [] = []
parse (x:y:vx:vy:rest) = (x,y,vx,vy) : parse rest

result nx ny t = product . foldr collect [0,0,0,0]
  where
    xmid = nx `div` 2
    ymid = ny `div` 2
    collect (x,y,vx,vy) [q1,q2,q3,q4]
      | x1 < xmid && y1 < ymid = [q1+1,q2,q3,q4]
      | x1 > xmid && y1 < ymid = [q1,q2+1,q3,q4]
      | x1 < xmid && y1 > ymid = [q1,q2,q3+1,q4]
      | x1 > xmid && y1 > ymid = [q1,q2,q3,q4+1]
      | otherwise = [q1,q2,q3,q4]
      where
        x1 = (x+vx*t) `mod` nx
        y1 = (y+vy*t) `mod` ny

draw t = readFile "input/14.txt" >>= putStr . show2ds . makeSet t . parse . parseInts

metric set = -sum [length [() | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0), member (x+dx,y+dy) set]
                   | (x,y) <- toList set]

makeSet t = fromList . map toXY
  where
    nx = 101
    ny = 103
    toXY (x,y,vx,vy) = ((x+vx*t) `mod` nx,(y+vy*t) `mod` ny)

result2 input = snd $ minimum [(metric $ makeSet (x+101*t) input,x+101*t) | t <- [1..103]]
  where
    x = snd $ minimum [(metric $ makeSet t input,t) | t <- [1..101]]
