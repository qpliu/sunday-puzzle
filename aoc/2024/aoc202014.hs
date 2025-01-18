module AOC202014 where

import Data.Bits(complement,(.&.),(.|.))
import Data.Map(Map,empty,insert)

import AOC

aoc = AOC {
    day="../../2020/input/14",
    aocTests=[
        AOCTest {
            testData=unlines [
                "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
                "mem[8] = 11",
                "mem[7] = 101",
                "mem[8] = 0"
                ],
            testResult=Just "165",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "mask = 000000000000000000000000000000X1001X",
                "mem[42] = 100",
                "mask = 00000000000000000000000000000000X0XX",
                "mem[26] = 1"
                ],
            testResult=Nothing,
            testResult2=Just "208"
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

parse :: String -> [Either (Int,Int) [Int]]
parse = map p . lines
  where
    p ('m':'a':'s':'k':' ':'=':' ':mask) = Left $ parseMask mask
    p mem = Right $ parseInts mem
    parseMask mask =
        (sum [2^bit | (bit,m) <- zip [35,34..] mask, m == '1'],
         sum [2^bit | (bit,m) <- zip [35,34..] mask, m == 'X'])

execute :: ((Int,Int),Map Int Int) -> Either (Int,Int) [Int]
        -> ((Int,Int),Map Int Int)
execute (_,memory) (Left mask) = (mask,memory)
execute (mask@(mask1,maskX),memory) (Right [address,value]) =
    (mask,insert address ((value .&. maskX) .|. mask1) memory)

result = sum . snd . foldl execute ((0,0),empty)

-- There are a maximum of 9 floating bits in a mask in my input.

-- Expanding out the floating bits with a maximum of 7 writes per mask,
-- gives an upper limit of less than 130000 addresses written to for my
-- input, so might as well just brute-force expand out all the possible
-- floating bits.  And it turns out to be less than 80000 addresses that
-- are written to for my input.

parse2 :: String -> [Either (Int,Int,[Int]) [Int]]
parse2 = map p . lines
  where
    p ('m':'a':'s':'k':' ':'=':' ':mask) = Left $ parseMask mask
    p mem = Right $ parseInts mem
    parseMask mask =
        (sum [2^bit | (bit,m) <- zip [35,34..] mask, m == '1'],
         complement $ sum [2^bit | (bit,m) <- zip [35,34..] mask, m == 'X'],
         floating [2^bit | (bit,m) <- zip [35,34..] mask, m == 'X'])
    floating [] = [0]
    floating (bit:bits) = map (bit+) (floating bits) ++ floating bits

execute2 :: ((Int,Int,[Int]),Map Int Int) -> Either (Int,Int,[Int]) [Int]
         -> ((Int,Int,[Int]),Map Int Int)
execute2 (_,memory) (Left mask) = (mask,memory)
execute2 (mask@(mask1,maskX,floating),memory) (Right [address,value]) =
    (mask,foldr store memory floating)
  where store float = insert ((address .&. maskX) .|. mask1 .|. float) value

result2 = sum . snd . foldl execute2 ((0,0,[]),empty)
