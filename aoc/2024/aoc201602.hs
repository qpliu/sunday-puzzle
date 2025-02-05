module AOC201602 where

import AOC

aoc = AOC {
    day="../../2016/input/02",
    aocTests=[
        AOCTest {
            testData=unlines [
                "ULL",
                "RRDDD",
                "LURDL",
                "UUUUD"
                ],
            testResult=Just $ show "1985",
            testResult2=Just $ show "5DB3"
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

parse = lines

move :: (Int,Int) -> Char -> (Int,Int)
move (x,y) 'U' = (x,y-1)
move (x,y) 'D' = (x,y+1)
move (x,y) 'L' = (x-1,y)
move (x,y) 'R' = (x+1,y)

button1 :: (Int,Int) -> Char
button1 (-1,-1) = '1'
button1 (0,-1) = '2'
button1 (1,-1) = '3'
button1 (-1,0) = '4'
button1 (0,0) = '5'
button1 (1,0) = '6'
button1 (-1,1) = '7'
button1 (0,1) = '8'
button1 (1,1) = '9'

move1 :: (Int,Int) -> Char -> (Int,Int)
move1 xy dir
  | abs x <= 1 && abs y <= 1 = newXY
  | otherwise = xy
  where newXY@(x,y) = move xy dir

follow :: ((Int,Int) -> Char -> (Int,Int)) -> ((Int,Int) -> Char)
       -> (Int,Int) -> [String] -> String
follow move button xy [] = [button xy]
follow move button xy (s:ss) =
    button xy : follow move button (foldl move xy s) ss

result = tail . follow move1 button1 (0,0)

button2 :: (Int,Int) -> Char
button2 (-2,0) = '5'
button2 (-1,-1) = '2'
button2 (-1,0) = '6'
button2 (-1,1) = 'A'
button2 (0,-2) = '1'
button2 (0,-1) = '3'
button2 (0,0) = '7'
button2 (0,1) = 'B'
button2 (0,2) = 'D'
button2 (1,-1) = '4'
button2 (1,0) = '8'
button2 (1,1) = 'C'
button2 (2,0) = '9'

move2 :: (Int,Int) -> Char -> (Int,Int)
move2 xy dir
  | dist newXY <= 2 = newXY
  | otherwise = xy
  where newXY = move xy dir

dist :: (Int,Int) -> Int
dist (x,y) = abs x + abs y

result2 = tail . follow move2 button2 (-2,0)
