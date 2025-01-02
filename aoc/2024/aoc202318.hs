module AOC202318 where

import Data.Char(ord)

import AOC

aoc = AOC {
    day="../../2023/input/18",
    aocTests=[
        AOCTest {
            testData=unlines [
                "R 6 (#70c710)",
                "D 5 (#0dc571)",
                "L 2 (#5713f0)",
                "D 2 (#d2c081)",
                "R 2 (#59c680)",
                "D 2 (#411b91)",
                "L 5 (#8ceee2)",
                "U 2 (#caa173)",
                "L 1 (#1b58a2)",
                "U 2 (#caa171)",
                "R 2 (#7807d2)",
                "U 3 (#a77fa3)",
                "L 2 (#015232)",
                "U 2 (#7a21e3)"
                ],
            testResult=Just "62",
            testResult2=Just "952408144115"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result,
        codeResult=result,
        codeResult2=result
        }
    }

parse :: String -> [(String,Int)]
parse = map (p . words) . lines
  where p (dir:n:_) = (dir,read n)

parse2 :: String -> [(String,Int)]
parse2 = map (p . words) . lines
  where
    p [_,_,['(','#',a,b,c,d,e,f,')']] =
        (dir f,16^4*n a+16^3*n b+16^2*n c+16*n d+n e)
    dir '0' = "R"
    dir '1' = "D"
    dir '2' = "L"
    dir '3' = "U"
    n c | c >= '0' && c <= '9' = ord c - ord '0'
        | otherwise = 10 + ord c - ord 'a'

trench :: [(String,Int)] -> [(Int,Int)]
trench = follow (0,0) "U"
  where
    follow (0,0) "U" [] = [(0,0)]
    follow (x,y) "R" (("U",n):rest) = (x,y) : follow (x,y-n+1) "U" rest
    follow (x,y) "R" (("D",n):rest) = (x+1,y) : follow (x+1,y+n) "D" rest
    follow (x,y) "L" (("U",n):rest) = (x-1,y) : follow (x-1,y-n) "U" rest
    follow (x,y) "L" (("D",n):rest) = (x,y) : follow (x,y+n-1) "D" rest
    follow (x,y) "U" (("L",n):rest) = (x,y) : follow (x-n+1,y) "L" rest
    follow (x,y) "U" (("R",n):rest) = (x,y-1) : follow (x+n,y-1) "R" rest
    follow (x,y) "D" (("L",n):rest) = (x,y+1) : follow (x-n,y+1) "L" rest
    follow (x,y) "D" (("R",n):rest) = (x,y) : follow (x+n-1,y) "R" rest
    follow xy dir rest = error (show (xy,dir,rest))

area :: [(Int,Int)] -> Int
area pts =
    sum (zipWith3 f (drop 1 pts ++ take 1 pts) pts (last pts:init pts)) `div` 2
  where f (xm,_) (_,y) (xp,_) = y*(xp-xm)

result = area . trench
