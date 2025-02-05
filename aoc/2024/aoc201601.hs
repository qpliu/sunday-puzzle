module AOC201601 where

import Data.Set(Set,empty,insert,member)

import AOC

aoc = AOC {
    day="../../2016/input/01",
    aocTests=[
        AOCTest {
            testData="R2, L3",
            testResult=Just "5",
            testResult2=Nothing
            },
        AOCTest {
            testData="R2, R2, R2",
            testResult=Just "2",
            testResult2=Nothing
            },
        AOCTest {
            testData="R5, L5, R5, R3",
            testResult=Just "12",
            testResult2=Nothing
            },
        AOCTest {
            testData="R8, R4, R4, R8",
            testResult=Nothing,
            testResult2=Just "4"
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

parse :: String -> [(Int,Int)]
parse = walk (0,0) (0,-1) . words
  where
    walk xy@(x,y) (dx,dy) [] = [xy]
    walk xy@(x,y) (dx,dy) (('R':move):moves) =
        xy:walk (x-n*dy,y+n*dx) (-dy,dx) moves
      where [n] = parseInts move
    walk xy@(x,y) (dx,dy) (('L':move):moves) =
        xy:walk (x+n*dy,y-n*dx) (dy,-dx) moves
      where [n] = parseInts move

dist :: (Int,Int) -> Int
dist (x,y) = abs x + abs y

result = dist . last

parse2 :: String -> [(Int,Int)]
parse2 = walk (0,0) (0,-1) . words
  where
    walk xy@(x,y) (dx,dy) [] = [xy]
    walk xy@(x,y) (dx,dy) (('R':move):moves) =
        [(x-i*dy,y+i*dx) | i <- [1..n]] ++ walk (x-n*dy,y+n*dx) (-dy,dx) moves
      where [n] = parseInts move
    walk xy@(x,y) (dx,dy) (('L':move):moves) =
        [(x+i*dy,y-i*dx) | i <- [1..n]] ++ walk (x+n*dy,y-n*dx) (dy,-dx) moves
      where [n] = parseInts move

findDuplicate :: Ord a => Set a -> [a] -> a
findDuplicate set (a:as)
  | member a set = a
  | otherwise = findDuplicate (insert a set) as

result2 = dist . findDuplicate empty
