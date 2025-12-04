module AOC202504 where

import Data.Bits(complement,popCount,shiftL,shiftR,xor,(.&.),(.|.))

import AOC

aoc = AOC {
    day="04",
    aocTests=[
        AOCTest {
            testData=unlines [
                "..@@.@@@@.",
                "@@@.@.@.@@",
                "@@@@@.@.@@",
                "@.@@@@..@.",
                "@@.@@@@.@@",
                ".@@@@@@@.@",
                ".@.@.@.@@@",
                "@.@@@.@@@@",
                ".@@@@@@@@.",
                "@.@.@@@.@."
            ],
            testResult=Just "13",
            testResult2=Just "43"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2 0,
        codeResult=result,
        codeResult2=result2 0
        }
    }

parse :: String -> [Integer]
parse = map (p 1 0) . lines
  where
    p bit bitset [] = bitset
    p bit bitset ('@':rest) = p (shiftL bit 1) (bitset .|. bit) rest
    p bit bitset (_:rest) = p (shiftL bit 1) bitset rest

moveable :: [Integer] -> [Integer]
moveable bitsets = zipWith3 f bitsets (0:bitsets) (drop 1 bitsets ++ [0])
  where
    f b b2 b7 =
        b .&. complement (    (b1 .&. b2 .&. b3 .&. b4)
                          .|. (b1 .&. b2 .&. b3 .&. b5)
                          .|. (b1 .&. b2 .&. b3 .&. b6)
                          .|. (b1 .&. b2 .&. b3 .&. b7)
                          .|. (b1 .&. b2 .&. b3 .&. b8)
                          .|. (b1 .&. b2 .&. b4 .&. b5)
                          .|. (b1 .&. b2 .&. b4 .&. b6)
                          .|. (b1 .&. b2 .&. b4 .&. b7)
                          .|. (b1 .&. b2 .&. b4 .&. b8)
                          .|. (b1 .&. b2 .&. b5 .&. b6)
                          .|. (b1 .&. b2 .&. b5 .&. b7)
                          .|. (b1 .&. b2 .&. b5 .&. b8)
                          .|. (b1 .&. b2 .&. b6 .&. b7)
                          .|. (b1 .&. b2 .&. b6 .&. b8)
                          .|. (b1 .&. b2 .&. b7 .&. b8)
                          .|. (b1 .&. b3 .&. b4 .&. b5)
                          .|. (b1 .&. b3 .&. b4 .&. b6)
                          .|. (b1 .&. b3 .&. b4 .&. b7)
                          .|. (b1 .&. b3 .&. b4 .&. b8)
                          .|. (b1 .&. b3 .&. b5 .&. b6)
                          .|. (b1 .&. b3 .&. b5 .&. b7)
                          .|. (b1 .&. b3 .&. b5 .&. b8)
                          .|. (b1 .&. b3 .&. b6 .&. b7)
                          .|. (b1 .&. b3 .&. b6 .&. b8)
                          .|. (b1 .&. b3 .&. b7 .&. b8)
                          .|. (b1 .&. b4 .&. b5 .&. b6)
                          .|. (b1 .&. b4 .&. b5 .&. b7)
                          .|. (b1 .&. b4 .&. b5 .&. b8)
                          .|. (b1 .&. b4 .&. b6 .&. b7)
                          .|. (b1 .&. b4 .&. b6 .&. b8)
                          .|. (b1 .&. b4 .&. b7 .&. b8)
                          .|. (b1 .&. b5 .&. b6 .&. b7)
                          .|. (b1 .&. b5 .&. b6 .&. b8)
                          .|. (b1 .&. b5 .&. b7 .&. b8)
                          .|. (b1 .&. b6 .&. b7 .&. b8)
                          .|. (b2 .&. b3 .&. b4 .&. b5)
                          .|. (b2 .&. b3 .&. b4 .&. b6)
                          .|. (b2 .&. b3 .&. b4 .&. b7)
                          .|. (b2 .&. b3 .&. b4 .&. b8)
                          .|. (b2 .&. b3 .&. b5 .&. b6)
                          .|. (b2 .&. b3 .&. b5 .&. b7)
                          .|. (b2 .&. b3 .&. b5 .&. b8)
                          .|. (b2 .&. b3 .&. b6 .&. b7)
                          .|. (b2 .&. b3 .&. b6 .&. b8)
                          .|. (b2 .&. b3 .&. b7 .&. b8)
                          .|. (b2 .&. b4 .&. b5 .&. b6)
                          .|. (b2 .&. b4 .&. b5 .&. b7)
                          .|. (b2 .&. b4 .&. b5 .&. b8)
                          .|. (b2 .&. b4 .&. b6 .&. b7)
                          .|. (b2 .&. b4 .&. b6 .&. b8)
                          .|. (b2 .&. b4 .&. b7 .&. b8)
                          .|. (b2 .&. b5 .&. b6 .&. b7)
                          .|. (b2 .&. b5 .&. b6 .&. b8)
                          .|. (b2 .&. b5 .&. b7 .&. b8)
                          .|. (b2 .&. b6 .&. b7 .&. b8)
                          .|. (b3 .&. b4 .&. b5 .&. b6)
                          .|. (b3 .&. b4 .&. b5 .&. b7)
                          .|. (b3 .&. b4 .&. b5 .&. b8)
                          .|. (b3 .&. b4 .&. b6 .&. b7)
                          .|. (b3 .&. b4 .&. b6 .&. b8)
                          .|. (b3 .&. b4 .&. b7 .&. b8)
                          .|. (b3 .&. b5 .&. b6 .&. b7)
                          .|. (b3 .&. b5 .&. b6 .&. b8)
                          .|. (b3 .&. b5 .&. b7 .&. b8)
                          .|. (b3 .&. b6 .&. b7 .&. b8)
                          .|. (b4 .&. b5 .&. b6 .&. b7)
                          .|. (b4 .&. b5 .&. b6 .&. b8)
                          .|. (b4 .&. b5 .&. b7 .&. b8)
                          .|. (b4 .&. b6 .&. b7 .&. b8)
                          .|. (b5 .&. b6 .&. b7 .&. b8))
      where
        b1 = shiftR b2 1
        b3 = shiftL b2 1
        b4 = shiftR b 1
        b5 = shiftL b 1
        b6 = shiftR b7 1
        b8 = shiftL b7 1

result = sum . map popCount . moveable

result2 n grid
  | removeCount == 0 = n
  | otherwise = result2 (n + removeCount) (zipWith xor grid removals)
  where
    removals = moveable grid
    removeCount = sum $ map popCount removals
