module AOC201722 where

import Data.Map(Map,delete,findWithDefault,insert,keys)

import AOC

aoc = AOC {
    day="../../2017/input/22",
    aocTests=[
        AOCTest {
            testData=unlines [
                "..#",
                "#..",
                "..."
                ],
            testResult=Just "5587",
            testResult2=Just "2511944"
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

parse input = (grid,(xmax`div`2,ymax`div`2))
  where
    grid = parse2d input
    (xmax,ymax) = maximum $ keys grid

virus :: Int -> Int -> (Int,Int) -> (Map (Int,Int) Char,(Int,Int)) -> Int
virus steps infections dir@(dx,dy) (grid,xy@(x,y))
  | steps <= 0 = infections
  | findWithDefault '.' xy grid == '.' =
      virus (steps-1) (infections+1) (dy,-dx) (insert xy '#' grid,(x+dy,y-dx))
  | otherwise =
      virus (steps-1) infections (-dy,dx) (delete xy grid,(x-dy,y+dx))

result = virus 10000 0 (0,-1)

virus2 :: Int -> Int -> (Int,Int) -> (Map (Int,Int) Char,(Int,Int)) -> Int
virus2 steps infections dir@(dx,dy) (grid,xy@(x,y))
  | steps <= 0 = infections
  | node == '.' =
      virus2 (steps-1) infections (dy,-dx) (insert xy 'W' grid,(x+dy,y-dx))
  | node == 'W' =
      virus2 (steps-1) (infections+1) (dx,dy) (insert xy '#' grid,(x+dx,y+dy))
  | node == '#' =
      virus2 (steps-1) infections (-dy,dx) (insert xy 'F' grid,(x-dy,y+dx))
  | node == 'F' =
      virus2 (steps-1) infections (-dx,-dy) (delete xy grid,(x-dx,y-dy))
  where node = findWithDefault '.' xy grid

result2 = virus2 10000000 0 (0,-1)
