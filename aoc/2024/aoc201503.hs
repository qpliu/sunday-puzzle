module AOC201503 where

import Data.List(partition)
import Data.Set(Set,fromList,size,unions)

import AOC

aoc = AOC {
    day="../../2015/input/03",
    aocTests=[
        AOCTest {
            testData=unlines [
                "^v^v^v^v^v"
                ],
            testResult=Just "2",
            testResult2=Just "11"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

walk :: (Int,Int) -> String -> [(Int,Int)]
walk xy [] = [xy]
walk xy@(x,y) ('<':rest) = xy : walk (x-1,y) rest
walk xy@(x,y) ('^':rest) = xy : walk (x,y-1) rest
walk xy@(x,y) ('>':rest) = xy : walk (x+1,y) rest
walk xy@(x,y) ('v':rest) = xy : walk (x,y+1) rest
walk xy (_:rest) = xy : walk xy rest

parse = fromList . walk (0,0)

parse2 = p . partition (even . fst) . zip [0..]
  where p (santa,robo) = [parse $ map snd santa,parse $ map snd robo]

result = size

result2 = size . unions
