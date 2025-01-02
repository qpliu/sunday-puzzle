module AOC202310 where

import Data.Map(Map,empty,insert,keys,member,toList,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2023/input/10",
    aocTests=[
        AOCTest {
            testData=unlines [
                ".....",
                ".S-7.",
                ".|.|.",
                ".L-J.",
                "....."
                ],
            testResult=Just "4",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "7-F7-",
                ".FJ|7",
                "SJLL7",
                "|F--J",
                "LJ.LJ"
                ],
            testResult=Just "8",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "...........",
                ".S-------7.",
                ".|F-----7|.",
                ".||.....||.",
                ".||.....||.",
                ".|L-7.F-J|.",
                ".|..|.|..|.",
                ".L--J.L--J.",
                "..........."
                ],
            testResult=Nothing,
            testResult2=Just "4"
            },
        AOCTest {
            testData=unlines [
                "..........",
                ".S------7.",
                ".|F----7|.",
                ".||OOOO||.",
                ".||OOOO||.",
                ".|L-7F-J|.",
                ".|II||II|.",
                ".L--JL--J.",
                ".........."
                ],
            testResult=Nothing,
            testResult2=Just "4"
            },
        AOCTest {
            testData=unlines [
                ".F----7F7F7F7F-7....",
                ".|F--7||||||||FJ....",
                ".||.FJ||||||||L7....",
                "FJL7L7LJLJ||LJ.L-7..",
                "L--J.L7...LJS7F-7L7.",
                "....F-J..F7FJ|L7L7L7",
                "....L7.F7||L7|.L7L7|",
                ".....|FJLJ|FJ|F7|.LJ",
                "....FJL-7.||.||||...",
                "....L---J.LJ.LJLJ..."
                ],
            testResult=Nothing,
            testResult2=Just "8"
            },
        AOCTest {
            testData=unlines [
                "FF7FSF7F7F7F7F7F---7",
                "L|LJ||||||||||||F--J",
                "FL-7LJLJ||||||LJL-77",
                "F--JF--7||LJLJ7F7FJ-",
                "L---JF-JLJ.||-FJLJJ7",
                "|F|F-JF---7F7-L7L|7|",
                "|FFJF7L7F-JF7|JL---7",
                "7-L-JL7||F7|L7F-7F7|",
                "L.L7LFJ|||||FJL7||LJ",
                "L7JLJL-JLJLJL--JLJ.L"
                ],
            testResult=Nothing,
            testResult2=Just "10"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const (start . parse2d),
        pcodeParse2=const (start . parse2d),
        pcodeTest=const result,
        pcodeTest2=result2,
        pcodeResult=const result,
        pcodeResult2=result2
        }
    }

start :: Map (Int,Int) Char -> (Map (Int,Int) Char,(Int,Int),Map (Int,Int) Int)
start grid = (fixedGrid,xy,mapPipes fixedGrid xy)
  where
    fixedGrid = insert xy (pipe connections) grid
    xy@(x,y) = fst $ head $ filter ((== 'S') . snd) $ toList grid
    connections =
        [maybe False (`elem` connects) $ Data.Map.lookup (x+dx,y+dy) grid
         | (dx,dy,connects) <- [(1,0,"-J7"),
                                (-1,0,"-FL"),
                                (0,1,"|JL"),
                                (0,-1,"|F7")]]
    pipe [True,True,False,False] = '-'
    pipe [True,False,True,False] = 'F'
    pipe [True,False,False,True] = 'L'
    pipe [False,True,True,False] = '7'
    pipe [False,True,False,True] = 'J'
    pipe [False,False,True,True] = '|'


mapPipes :: Map (Int,Int) Char -> (Int,Int) -> Map (Int,Int) Int
mapPipes grid startXY = bfs empty [(0,startXY)]
  where
    bfs pipes [] = pipes
    bfs pipes ((nsteps,xy@(x,y)):queue)
      | member xy pipes = bfs pipes queue
      | otherwise = bfs (insert xy nsteps pipes) (queue ++ enqueue (grid!xy))
      where
        enqueue '-' = [(nsteps+1,(x-1,y)),(nsteps+1,(x+1,y))]
        enqueue 'F' = [(nsteps+1,(x+1,y)),(nsteps+1,(x,y+1))]
        enqueue 'L' = [(nsteps+1,(x+1,y)),(nsteps+1,(x,y-1))]
        enqueue '7' = [(nsteps+1,(x-1,y)),(nsteps+1,(x,y+1))]
        enqueue 'J' = [(nsteps+1,(x-1,y)),(nsteps+1,(x,y-1))]
        enqueue '|' = [(nsteps+1,(x,y-1)),(nsteps+1,(x,y+1))]

result (grid,start,pipes) = maximum pipes

countEnclosed :: Int -> Map (Int,Int) Char -> Map (Int,Int) a -> Int
countEnclosed ncpu grid loop = parallelMapReduce ncpu countRow sum [ymin..ymax]
  where
    (xmin,xmax,ymin,ymax) = foldr getBounds (maxBound,0,maxBound,0) $ keys loop
    getBounds (x,y) (x0,x1,y0,y1) = (min x0 x,max x1 x,min y0 y,max y1 y)

    countRow y = countOut y 0 xmin

    countOut y n x
      | x > xmax = n
      | not (member (x,y) loop) = countOut y n (x+1)
      | grid!(x,y) == '|' = countIn y n (x+1)
      | grid!(x,y) == 'F' = countTop y n (x+1)
      | grid!(x,y) == 'L' = countBot y n (x+1)
    countIn y n x
      | x > xmax = error (show (x,xmax))
      | not (member (x,y) loop) = countIn y (n+1) (x+1)
      | grid!(x,y) == '|' = countOut y n (x+1)
      | grid!(x,y) == 'F' = countBot y n (x+1)
      | grid!(x,y) == 'L' = countTop y n (x+1)
    countTop y n x
      | x > xmax = error (show (x,xmax))
      | grid!(x,y) == '-' = countTop y n (x+1)
      | grid!(x,y) == 'J' = countIn y n (x+1)
      | grid!(x,y) == '7' = countOut y n (x+1)
    countBot y n x
      | x > xmax = error (show (x,xmax))
      | grid!(x,y) == '-' = countBot y n (x+1)
      | grid!(x,y) == 'J' = countOut y n (x+1)
      | grid!(x,y) == '7' = countIn y n (x+1)

result2 ncpu (grid,start,pipes) = countEnclosed ncpu grid pipes
