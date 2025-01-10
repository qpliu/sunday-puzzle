module AOC202208 where

import Data.Array(Array,assocs,bounds,inRange,(!))
import Data.Set(Set,empty,fromList,insert,size)

import AOC

aoc = AOC {
    day="../../2022/input/08",
    aocTests=[
        AOCTest {
            testData=unlines [
                "30373",
                "25512",
                "65332",
                "33549",
                "35390"
                ],
            testResult=Just "21",
            testResult2=Just "8"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse2da,
        pcodeParse2=const parse2da,
        pcodeTest=const result,
        pcodeTest2=result2,
        pcodeResult=const result,
        pcodeResult2=result2
        }
    }

visible :: Array (Int,Int) Char -> Set (Int,Int)
visible grid =
    fromList $ concat
             $    [from (x,ymin) (0,1)  (pred '0') | x <- [xmin..xmax]]
               ++ [from (x,ymax) (0,-1) (pred '0') | x <- [xmin..xmax]]
               ++ [from (xmin,y) (1,0)  (pred '0') | y <- [ymin..ymax]]
               ++ [from (xmax,y) (-1,0) (pred '0') | y <- [ymin..ymax]]
  where
    range@((xmin,ymin),(xmax,ymax)) = bounds grid
    inBounds = inRange range
    from xy@(x,y) dxy@(dx,dy) tallest
      | not (inBounds xy) = []
      | current > tallest = xy : from (x+dx,y+dy) dxy current
      | otherwise = from (x+dx,y+dy) dxy tallest
      where current = grid!xy

result = size . visible

score :: Array (Int,Int) Char -> ((Int,Int),(Int,Int)) -> ((Int,Int),Char)
      -> Int
score grid ((xmin,ymin),(xmax,ymax)) (xy@(x,y),tree)
  | x == xmin || x == xmax || y == ymin || y == ymax = 0
  | otherwise = product [look (x+1,y) (1,0)  ((> xmax) . fst) 0,
                         look (x-1,y) (-1,0) ((< xmin) . fst) 0,
                         look (x,y+1) (0,1)  ((> ymax) . snd) 0,
                         look (x,y-1) (0,-1) ((< ymin) . snd) 0]
  where
    look xy@(x,y) dxy@(dx,dy) out n
      | out xy = n
      | tree <= grid!xy = n+1
      | otherwise = look (x+dx,y+dy) dxy out (n+1)

result2 ncpu grid =
    parallelMapReduce ncpu (score grid (bounds grid)) maximum $ assocs grid
