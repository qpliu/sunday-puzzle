module AOC201711 where

import AOC

aoc = AOC {
    day="../../2017/input/11",
    aocTests=[
        AOCTest {
            testData="ne,ne,ne",
            testResult=Just "3",
            testResult2=Nothing
            },
        AOCTest {
            testData="ne,ne,sw,sw",
            testResult=Just "0",
            testResult2=Nothing
            },
        AOCTest {
            testData="ne,ne,s,s",
            testResult=Just "2",
            testResult2=Nothing
            },
        AOCTest {
            testData="se,sw,se,sw,sw",
            testResult=Just "3",
            testResult2=Nothing
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

parse = (++ ",")

dist :: (Int,Int) -> Int
dist (x,y) = max 0 (abs y-(abs x+1) `div` 2) + abs x

follow :: (Int,Int) -> String -> [(Int,Int)]
follow xy@(x,y) ('n':',':rest) = xy:follow (x,y-1) rest
follow xy@(x,y) ('n':'e':',':rest)
  | even x = xy:follow (x+1,y) rest
  | odd x = xy:follow (x+1,y-1) rest
follow xy@(x,y) ('s':'e':',':rest)
  | even x = xy:follow (x+1,y+1) rest
  | odd x = xy:follow (x+1,y) rest
follow xy@(x,y) ('s':',':rest) = xy:follow (x,y+1) rest
follow xy@(x,y) ('s':'w':',':rest)
  | even x = xy:follow (x-1,y+1) rest
  | odd x = xy:follow (x-1,y) rest
follow xy@(x,y) ('n':'w':',':rest)
  | even x = xy:follow (x-1,y) rest
  | odd x = xy:follow (x-1,y-1) rest
follow xy _ = [xy]

result = dist . last . follow (0,0)

result2 = maximum . map dist . follow (0,0)
