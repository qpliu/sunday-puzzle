module AOC202406 where

import Data.Array(Array,Ix,array,assocs,bounds,inRange,range,(!))
import Data.Set(Set,empty,insert,member,singleton,size)

import AOC

aoc = AOC {
    day="06",
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
    testResult="41",
    testData2="",
    testResult2="6",
    aocParse=parse2da,
    aocResult=result,
    aocParse2=parse2da,
    aocResult2=result2
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

result2 :: Array (Int,Int) Char -> Int
result2 grid = walk empty empty (start grid) N
  where
    out = not . inRange (bounds grid)
    graph = makeGraph grid
    walk loop noloop xy dir
      | out nextXY = size loop
      | grid!nextXY == '#' = walk loop noloop xy (turn dir)
      | member nextXY loop || member nextXY noloop =
          walk loop noloop nextXY dir
      | hasLoop graph xy dir =
          walk (insert nextXY loop) noloop nextXY dir
      | otherwise =
          walk loop (insert nextXY noloop) nextXY dir
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

hasLoop :: Array ((Int,Int),Dir) (Int,Int) -> (Int,Int) -> Dir -> Bool
hasLoop graph xy dir = walk empty (xy,dir)
  where
    out = not . inRange (bounds graph)
    (xobs,yobs) = step dir xy
    walk path idx@((x,y),dir)
      | out idx = False
      | member idx path = True
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
