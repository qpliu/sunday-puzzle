module AOC201902 where

import Data.Vector.Unboxed(Vector,(//))

import AOC
import AOC2019

aoc = AOC {
    day="../../2019/input/02",
    aocTests=[
        AOCTest {
            testData=unlines [
                "1,9,10,3,2,3,11,0,99,30,40,50"
                ],
            testResult=Nothing, --Just "[3500,9,10,70,2,3,11,0,99,30,40,50]",
            testResult2=Nothing
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parseIntCode,
        pcodeParse2=const parseIntCode,
        pcodeTest=undefined,
        pcodeTest2=undefined,
        pcodeResult=const result,
        pcodeResult2=result2
        }
    }

exec :: Vector Int -> Int -> Int -> Int
exec mem noun verb = intCode0 (mem // [(1,noun),(2,verb)])

result mem = exec mem 12 2

result2 ncpu mem =
    parallelMapReduce ncpu id maximum
                      [if exec mem noun verb == 19690720
                         then 100*noun+verb
                         else 0
                       | noun <- [0..99], verb <- [0..99]]
