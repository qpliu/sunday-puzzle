module AOC201821 where

import Data.Bits(shiftR,(.&.),(.|.))
import Data.Set(Set,empty,insert,member)

import AOC

aoc = AOC {
    day="../../2018/input/21",
    aocTests=[],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse = p . map parseInts . drop 8 . lines
  where p ([a,_,_]:_:_:_:[_,b,_]:_) = (a,b)

{-
 0 R3 = 123
 1 R3 = R3 AND 456 = 123 AND 456 = 72
 2 R3 = R3 == 72 = 1
 3 IF R3 GOTO 5
 4 GOTO 1
 5 R3 = 0
 6 R2 = R3 OR 0x10000
 7 R3 = PARAMETER A
 8 R1 = R2 AND 0xff
 9 R3 = R3 + R1
10 R3 = R3 AND 0xffffff
11 R3 = R3 * PARAMETER B
12 R3 = R3 AND 0xffffff
13 R1 = 256 > R2
14 IF R1 GOTO 16
15 GOTO 17
16 GOTO 28
17 R1 = 0
18 R4 = R1 + 1
19 R4 = R4 * 0x100
20 R4 = R4 > R2
21 IF R4 GOTO 23
22 GOTO 24
23 GOTO 26
24 R1 = R1 + 1
25 GOTO 18
26 R2 = R1
27 GOTO 8
28 R1 = R3 == R0
29 IF R1 HALT
30 GOTO 6

 5 R3 = 0
 6 LOOP
 6   R2 = R3 OR 0x10000
 7   R3 = PARAMETER A
 8   WHILE R2 > 0
12     R3 = ((R3 + (R2 AND 0xff))*PARAMETER B) AND 0xffffff
17     R2 = (R2 DIV 0x100)
27   END WHILE
28   IF R0 == R3 THEN HALT
30 END LOOP
-}

r3sequence :: (Int,Int) -> [Int]
r3sequence (a,b) = 0 : r3s 0
  where
    r3s r3 = nextR : r3s nextR
      where
        nextR = fst $ head $ dropWhile ((> 0) . snd)
                           $ iterate f (a,r3 .|. 0x10000)
    f (r3,r2) = (((r3 + (r2 .&. 0xff))*b) .&. 0xffffff, shiftR r2 8)

result = head . tail . r3sequence

findLastNonrepeat :: Ord a => Set a -> [a] -> a
findLastNonrepeat seen (a1:as@(a2:_))
  | member a2 seen = a1
  | otherwise = findLastNonrepeat (insert a2 seen) as

result2 = findLastNonrepeat empty . r3sequence
