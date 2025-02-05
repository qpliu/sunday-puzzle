module AOC201618 where

import Data.Bits(complement,popCount,setBit,shiftL,shiftR,xor,(.&.))

import AOC

aoc = AOC {
    day="../../2016/input/18",
    aocTests=[
        AOCTest {
            testData=".^^.^.^^^^",
            testResult=Just "38",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 10,
        codeTest2=undefined,
        codeResult=result 40,
        codeResult2=result 400000
        }
    }

parse :: String -> (Int,Integer,Integer)
parse input = (maxBit,mask,foldl setBit 0 bits)
  where
    tiles = filter (/= '\n') input
    maxBit = length tiles - 1
    mask = 2^(maxBit+1) - 1
    bits = map fst $ filter ((== '.') . snd) $ zip [0..] tiles

next :: Int -> Integer -> Integer -> Integer
next maxBit mask tiles =
    complement (setBit (shiftL tiles 1) 0 `xor` setBit (shiftR tiles 1) maxBit) .&. mask

result n (maxBit,mask,tiles) =
    sum $ map popCount $ take n $ iterate (next maxBit mask) tiles
