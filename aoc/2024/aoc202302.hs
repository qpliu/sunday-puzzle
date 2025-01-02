module AOC202302 where

import AOC

aoc = AOC {
    day="../../2023/input/02",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
                "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
                "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
                "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
                "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
                ],
            testResult=Just "8",
            testResult2=Just "2286"
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

parse :: String -> [(Int,[(Int,Int,Int)])]
parse = map (p . words) . lines
  where
    p ("Game":gameID:rest) = (read (init gameID),p2 [] (0,0,0) rest)
    p2 sets (r,g,b) [n,"red"] =
        (r+read n,g,b):sets
    p2 sets (r,g,b) [n,"green"] =
        (r,g+read n,b):sets
    p2 sets (r,g,b) [n,"blue"] =
        (r,g,b+read n):sets
    p2 sets (r,g,b) (n:"red,":rest) =
        p2 sets (r+read n,g,b) rest
    p2 sets (r,g,b) (n:"green,":rest) =
        p2 sets (r,g+read n,b) rest
    p2 sets (r,g,b) (n:"blue,":rest) =
        p2 sets (r,g,b+read n) rest
    p2 sets (r,g,b) (n:"red;":rest) =
        p2 ((r+read n,g,b):sets) (0,0,0) rest
    p2 sets (r,g,b) (n:"green;":rest) =
        p2 ((r,g+read n,b):sets) (0,0,0) rest
    p2 sets (r,g,b) (n:"blue;":rest) =
        p2 ((r,g,b+read n):sets) (0,0,0) rest

possible (r,g,b) = r <= 12 && g <= 13 && b <= 14

result = sum . map fst . filter (all possible . snd)

power sets = r*g*b
  where
    (r,g,b) = foldr maxes (0,0,0) sets
    maxes (r1,g1,b1) (r2,g2,b2) = (max r1 r2,max g1 g2,max b1 b2)

result2 = sum . map (power . snd)
