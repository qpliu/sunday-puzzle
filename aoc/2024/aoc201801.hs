module AOC201801 where

import Data.Set(Set,empty,insert,member)

import AOC

aoc = AOC {
    day="../../2018/input/01",
    aocTests=[
        AOCTest {
            testData="+1, -2, +3, +1",
            testResult=Just "3",
            testResult2=Just "2"
            },
        AOCTest {
            testData="+1, +1, +1",
            testResult=Just "3",
            testResult2=Nothing
            },
        AOCTest {
            testData="+1, +1, -2",
            testResult=Just "0",
            testResult2=Nothing
            },
        AOCTest {
            testData="-1, -2, -3",
            testResult=Just "-6",
            testResult2=Nothing
            },
        AOCTest {
            testData="+1, -1",
            testResult=Nothing,
            testResult2=Just "0"
            },
        AOCTest {
            testData="+3, +3, +4, -2, -4",
            testResult=Nothing,
            testResult2=Just "10"
            },
        AOCTest {
            testData="-6, +3, +8, +5, -6",
            testResult=Nothing,
            testResult2=Just "5"
            },
        AOCTest {
            testData="+7, +7, -2, -7, -4",
            testResult=Nothing,
            testResult2=Just "14"
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=parseInts,
        codeTest=sum,
        codeTest2=result2,
        codeResult=sum,
        codeResult2=result2
        }
    }

search :: Set Int -> Int -> [Int] -> Int
search seen total (n:ns)
  | member total seen = total
  | otherwise = search (insert total seen) (total+n) ns

result2 = search empty 0 . cycle
