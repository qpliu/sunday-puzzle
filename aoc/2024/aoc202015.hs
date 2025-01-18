module AOC202015 where

import Data.Map.Strict(Map,findWithDefault,fromList,insert)

import AOC

aoc = AOC {
    day="../../2020/input/15",
    aocTests=[
        AOCTest {
            testData="0,3,6",
            testResult=Just "436",
            testResult2=Nothing --Just "175594"
            },
        AOCTest {
            testData="1,3,2",
            testResult=Just "1",
            testResult2=Nothing --Just "2578"
            },
        AOCTest {
            testData="2,1,3",
            testResult=Just "10",
            testResult2=Nothing --Just "3544142"
            },
        AOCTest {
            testData="1,2,3",
            testResult=Just "27",
            testResult2=Nothing --Just "261214"
            },
        AOCTest {
            testData="2,3,1",
            testResult=Just "78",
            testResult2=Nothing --Just "6895259"
            },
        AOCTest {
            testData="3,2,1",
            testResult=Just "438",
            testResult2=Nothing --Just "18"
            },
        AOCTest {
            testData="3,1,2",
            testResult=Just "1836",
            testResult2=Nothing --Just "362"
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=parseInts,
        codeTest=result 2020,
        codeTest2=const (), --result 30000000,
        codeResult=result 2020,
        codeResult2=const () --result 30000000
        }
    }

speak :: Int -> (Int,(Int,Map Int Int)) -> Int
speak n (number,(turn,history))
  | turn >= n = number
  | otherwise = speak n (turn - findWithDefault turn number history,
                         (turn+1,insert number turn history))

starting :: [Int] -> (Int,(Int,Map Int Int))
starting numbers =
    (last numbers,(length numbers,fromList $ zip (init numbers) [1..]))

result n = speak n . starting

-- Part 2 results in stack overflows.
