module AOC202413 where

import AOC

aoc = AOC {
    day="13",
    testData=unlines [
    "Button A: X+94, Y+34",
    "Button B: X+22, Y+67",
    "Prize: X=8400, Y=5400",
    "",
    "Button A: X+26, Y+66",
    "Button B: X+67, Y+21",
    "Prize: X=12748, Y=12176",
    "",
    "Button A: X+17, Y+86",
    "Button B: X+84, Y+37",
    "Prize: X=7870, Y=6450",
    "",
    "Button A: X+69, Y+23",
    "Button B: X+27, Y+71",
    "Prize: X=18641, Y=10279"
    ],
    testResult="480",
    testData2="",
    testResult2="875318608908",
    aocParse=parse,
    aocTest=result 0,
    aocResult=result 0,
    aocParse2=parse,
    aocTest2=result 10000000000000,
    aocResult2=result 10000000000000
    }

parse :: String -> [((Int,Int),(Int,Int),(Int,Int))]
parse = p . parseInts
  where
    p [] = []
    p (ax:ay:bx:by:px:py:rest) = ((ax,ay),(bx,by),(px,py)) : p rest

-- na == -(by*px - bx*py)/(ay*bx - ax*by)
-- nb == (ay*px - ax*py)/(ay*bx - ax*by)
tokens :: Int -> ((Int,Int),(Int,Int),(Int,Int)) -> Int
tokens offset ((ax,ay),(bx,by),(px,py))
  | denom == 0 || anum `mod` denom /= 0 || bnum `mod` denom /= 0
               || na < 0 || nb < 0 = 0
  | otherwise = 3*na+nb
  where
    denom = ay*bx - ax*by
    anum = bx*(py+offset) - by*(px+offset)
    bnum = ay*(px+offset) - ax*(py+offset)
    na = anum `div` denom
    nb = bnum `div` denom

result offset = sum . map (tokens offset)
