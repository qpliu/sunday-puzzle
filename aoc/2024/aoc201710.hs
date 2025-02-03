module AOC201710 where

import Data.Vector.Unboxed(Vector,(!))
import qualified Data.Vector.Unboxed as V

import AOC
import AOC2017

aoc = AOC {
    day="../../2017/input/10",
    aocTests=[
        AOCTest {
            testData="",
            testResult=Nothing,
            testResult2=Just $ show "a2582a3a0e66e6e86e3812dcb672a272"
            },
        AOCTest {
            testData="AoC 2017",
            testResult=Nothing,
            testResult2=Just $ show "33efeb34ea91902bb2f59c9920caa6cd"
            },
        AOCTest {
            testData="1,2,3",
            testResult=Nothing,
            testResult2=Just $ show "3efbe78a8d82f29979031a4aa0b16a9d"
            },
        AOCTest {
            testData="1,2,4",
            testResult=Nothing,
            testResult2=Just $ show "63960835bcdc130f0b66d7ff4f6a5a8e"
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=filter (/= '\n'),
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

result input = k!0*k!1
  where k = knotSparse 1 input

hexDigit :: Int -> Char
hexDigit 0 = '0'
hexDigit 1 = '1'
hexDigit 2 = '2'
hexDigit 3 = '3'
hexDigit 4 = '4'
hexDigit 5 = '5'
hexDigit 6 = '6'
hexDigit 7 = '7'
hexDigit 8 = '8'
hexDigit 9 = '9'
hexDigit 10 = 'a'
hexDigit 11 = 'b'
hexDigit 12 = 'c'
hexDigit 13 = 'd'
hexDigit 14 = 'e'
hexDigit 15 = 'f'

hex :: Int -> Vector Int -> String
hex i v
  | i >= V.length v = []
  | otherwise = hexDigit (v!i `div` 16):hexDigit (v!i `mod` 16):hex (i+1) v

result2 = hex 0 . knot
