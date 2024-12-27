module AOC202406 where

import Data.Array(Array,Ix,array,assocs,bounds,inRange,range,(!))
import Data.Set(Set,empty,insert,member,singleton,size)

import AOC

aoc = AOC {
    day="06",
    aocTests=[
        AOCTest {
            testData=unlines [
                "....#.....",
                ".........#",
                "..........",
                "..#.......",
                ".......#..",
                "..........",
                ".#..^.....",
                "........#.",
                "#.........",
                "......#..."
                ],
            testResult=Just "41",
            testResult2=Just "6"
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

result :: Array (Int,Int) Char -> Int
result grid = size $ (toCounts grid)!((start grid),N)

start = s . assocs
  where
    s ((xy,'^'):_) = xy
    s (_:as) = s as

data Dir = N | E | S | W deriving (Bounded,Enum,Eq,Ix,Ord,Show)

turn dir
  | dir == maxBound = minBound
  | otherwise = succ dir

step N (x,y) = (x,y-1)
step E (x,y) = (x+1,y)
step S (x,y) = (x,y+1)
step W (x,y) = (x-1,y)

toCounts :: Array (Int,Int) Char -> Array ((Int,Int),Dir) (Set (Int,Int))
toCounts grid = countsArray
  where
    (minXY,maxXY) = bounds grid
    countsBounds = ((minXY,minBound),(maxXY,maxBound))
    countsArray = array countsBounds $ map counts $ range countsBounds
    counts idx@(xy,dir)
      | not (inRange (bounds grid) nextXY) = (idx,singleton xy)
      | grid!nextXY == '#' = (idx,countsArray!(xy,turn dir))
      | otherwise = (idx,insert xy (countsArray!(nextXY,dir)))
      where nextXY = step dir xy

result2 :: Int -> Array (Int,Int) Char -> Int
result2 ncpu grid =
    parallelMapReduce ncpu id sum $ walk empty (start grid) N
  where
    out = not . inRange (bounds grid)
    graph = makeGraph grid
    walk tried xy dir
      | out nextXY = []
      | grid!nextXY == '#' = walk tried xy (turn dir)
      | member nextXY tried = walk tried nextXY dir
      | otherwise =
          hasLoop graph xy dir : walk (insert nextXY tried) nextXY dir
      where nextXY = step dir xy

makeGraph :: Array (Int,Int) Char -> Array ((Int,Int),Dir) (Int,Int)
makeGraph grid = graph
  where
    (minXY,maxXY) = bounds grid
    out = not . inRange (bounds grid)
    graphBounds = ((minXY,minBound),(maxXY,maxBound))
    graph = array graphBounds $ map makeStep $ range graphBounds
    makeStep idx@(xy,dir)
      | out nextXY = (idx,nextXY)
      | grid!nextXY == '#' = (idx,xy)
      | otherwise = (idx,graph!(nextXY,dir))
      where nextXY = step dir xy

hasLoop :: Array ((Int,Int),Dir) (Int,Int) -> (Int,Int) -> Dir -> Int
hasLoop graph xy dir = walk empty (xy,dir)
  where
    out = not . inRange (bounds graph)
    (xobs,yobs) = step dir xy
    walk path idx@((x,y),dir)
      | out idx = 0
      | member idx path = 1
      | x == xobs && y < yobs && yobs <= nexty =
          walk (insert idx path) ((x,yobs-1),turn dir)
      | x == xobs && y > yobs && yobs >= nexty =
          walk (insert idx path) ((x,yobs+1),turn dir)
      | y == yobs && x < xobs && xobs <= nextx =
          walk (insert idx path) ((xobs-1,y),turn dir)
      | y == yobs && x > xobs && xobs >= nextx =
          walk (insert idx path) ((xobs+1,y),turn dir)
      | otherwise = walk (insert idx path) (nextXY,turn dir)
      where nextXY@(nextx,nexty) = graph!idx
