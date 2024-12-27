module AOC202414 where

import Data.List(sort)
import Data.Set(fromList)

import AOC

aoc = AOC {
    day="14",
    aocTests=[
        AOCTest {
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
            testResult=Just "12",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse . parseInts,
        codeParse2=parse . parseInts,
        codeTest=result 11 7 100,
        codeTest2=undefined,
        codeResult=result 101 103 100,
        codeResult2=result2
        }
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

draw t = getInput2 aoc >>= p2ds . fromList . map toXY
  where toXY (x,y,vx,vy) = ((x+vx*t) `mod` 101,(y+vy*t) `mod` 103)

result2 input = snd $ minimum [(metric t,t) | (_,a) <- c, (_,b) <- c, t <- [n a b]]
  where
    c = take 2 $ sort $ [(metric t,t) | t <- [0..102]]
    n a b = head [a + 101*x | x <- [0..100], y <- [0..102], a+101*x == b+103*y]
    metric t = result 101 103 t input
