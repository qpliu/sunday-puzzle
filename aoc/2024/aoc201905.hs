module AOC201905 where

import Data.Vector.Unboxed(Vector,(//),(!))

import AOC
import AOC2019

aoc = AOC {
    day="../../2019/input/05",
    aocTests=[],
    aocCode=Code {
        codeParse=parseIntCode,
        codeParse2=parseIntCode,
        codeTest=undefined,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

result = last . intCode [1]

result2 = head . intCode [5]
