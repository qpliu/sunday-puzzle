module AOC201909 where

import AOC
import AOC2019

aoc = AOC {
    day="../../2019/input/09",
    aocTests=[
        AOCTest {
            testData=unlines [
                "104,1125899906842624,99"
                ],
            testResult=Just "1125899906842624",
            testResult2=Just "1125899906842624"
            }
        ],
    aocCode=Code {
        codeParse=parseIntCode,
        codeParse2=parseIntCode,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

result = head . intCode [1]

result2 = head . intCode [2]
