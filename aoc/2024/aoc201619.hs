module AOC201619 where

import AOC

aoc = AOC {
    day="../../2016/input/19",
    aocTests=[
        AOCTest {
            testData="5",
            testResult=Just "3",
            testResult2=Just "2"
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

parse = head . parseInts

-- If elf E(n) gets all the presents when there are n elves,
-- then, when there are n+1 elves, elf 1 takes from elf 2,
-- and elf 3 starts with n elves, so 3 -> 1, 4 -> 2, ...
-- n+1 -> n-1, 1 -> n, 2 -> eliminated, so (E(n)+1) mod (n+1) + 1 -> E(n).
-- Or E(n+1) = (((E(n)+1) mod (n+1)) + 1) gets all the presents.
elf :: Int -> Int -> Int -> Int
elf e n target
  | n >= target = e
  | e == n = elf 1 (n+1) target
  | otherwise = elf (e+2) (n+1) target

-- If elf E(n) gets all the presents when there are n elves,
-- then, when there are 2n elves, elf 1 takes from elf 2, elf 3 takes from
-- elf 4, etc, elf 2n-1 takes from elf 2n, so 1 -> 1, 3 -> 2, 5 -> 3, ...
-- 2n-1 -> n, so E(2n) = 2E(n) - 1
-- And, when there are 2n+1 elves, elf 1 takes from elf 2, elf 3 takes from
-- elf 4, etc, elf 2n+1 takes from elf 1, so 3 -> 1, 5 -> 2, 7 -> 3, ...
-- 2n+1 -> n, so E(2n+1) = 2E(n) + 1
elf1 :: Int -> Int
elf1 1 = 1
elf1 n
  | even n = 2*elf1 (n`div`2) - 1
  | otherwise = 2*elf1 (n`div`2) + 1

result = elf1

-- If elf E(n) gets all the present when there are n elves,
-- then, when there are n+1 elves, elf 1 takes from elf floor((n+1)/2+1),
-- and elf 2 starts with n elves, so 2 -> 1, 3 -> 2, ...
-- floor((n+1)/2+1)-1 -> floor((n+1)/2+1), floor((n+1)/2+1) -> eliminated,
-- floor((n+1)/2+1)+1 -> floor((n+1)/2+1)-1,
-- floor((n+1)/2+1)+2 -> floor((n+1)/2+1), ..., n -> n-2, n+1 -> n-1, 1 -> n
elf2 :: Int -> Int -> Int -> Int
elf2 e n target
  | n >= target = e
  | e == n = elf2 1 (n+1) target
  | e < (n+1)`div`2 = elf2 (e+1) (n+1) target
  | otherwise = elf2 (e+2) (n+1) target

result2 = elf2 1 1
