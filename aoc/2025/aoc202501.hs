module AOC202501 where

import AOC

aoc = AOC {
    day="01",
    aocTests=[
        AOCTest {
            testData=unlines [
                "L68",
                "L30",
                "R48",
                "L5",
                "R60",
                "L55",
                "L1",
                "L99",
                "R14",
                "L82"
            ],
            testResult=Just "3",
            testResult2=Just "6"
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

parse = map p . words
  where
    p ('L':n) = -read n
    p ('R':n) = read n

result input = turn 0 50 input
  where
    check n d rotations
      | mod d 100 == 0 = turn (n+1) d rotations
      | otherwise = turn n d rotations
    turn n d [] = n
    turn n d (t:ts) = check n (d+t) ts

result2 input = turn 0 50 input
  where
    turn n d [] = n
    turn n d (t:ts)
      | t > 100 = turn (n+1) d (t-100:ts)
      | t < -100 = turn (n+1) d (t+100:ts)
      | d+t >= 100 = turn (n+1) (d+t-100) ts
      | d > 0 && d+t <= 0 = turn (n+1) (mod (d+t) 100) ts
      | otherwise = turn n (mod (d+t) 100) ts
