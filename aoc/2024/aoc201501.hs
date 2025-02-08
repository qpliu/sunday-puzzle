module AOC201501 where

import AOC

aoc = AOC {
    day="../../2015/input/01",
    aocTests=[
        AOCTest {
            testData=unlines [
                ")())())"
                ],
            testResult=Just "-3",
            testResult2=Just "1"
            }
        ],
    aocCode=Code {
        codeParse=id,
        codeParse2=zip [0..],
        codeTest=result 0,
        codeTest2=result2 0,
        codeResult=result 0,
        codeResult2=result2 0
        }
    }

result floor ('(':rest) = result (floor+1) rest
result floor (')':rest) = result (floor-1) rest
result floor _ = floor

result2 (-1) ((i,_):_)= i
result2 floor ((_,'('):rest) = result2 (floor+1) rest
result2 floor ((_,')'):rest) = result2 (floor-1) rest
