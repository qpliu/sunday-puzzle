module AOC201907 where

import Data.List(permutations)
import Data.Vector.Unboxed(Vector)

import AOC
import AOC2019

aoc = AOC {
    day="../../2019/input/07",
    aocTests=[
        AOCTest {
            testData=unlines [
                "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
                ],
            testResult=Just "43210",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "3,23,3,24,1002,24,10,24,1002,23,-1,23,",
                "101,5,23,23,1,24,23,23,4,23,99,0,0"
                ],
            testResult=Just "54321",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,",
                "1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
                ],
            testResult=Just "65210",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,",
                "27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
                ],
            testResult=Nothing,
            testResult2=Just "139629729"
            },
        AOCTest {
            testData=unlines [
                "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,",
                "-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,",
                "53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
                ],
            testResult=Nothing,
            testResult2=Just "18216"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parseIntCode,
        pcodeParse2=const parseIntCode,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

runACS :: Vector Int -> [Int] -> Int
runACS mem [a,b,c,d,e] = last outE
  where
    outA = intCode (a:0:outE) mem
    outB = intCode (b:outA) mem
    outC = intCode (c:outB) mem
    outD = intCode (d:outC) mem
    outE = intCode (e:outD) mem

result ncpu mem =
    parallelMapReduce ncpu (runACS mem) maximum $ permutations [0..4]

result2 ncpu mem =
    parallelMapReduce ncpu (runACS mem) maximum $ permutations [5..9]
