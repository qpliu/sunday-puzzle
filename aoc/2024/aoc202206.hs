module AOC202206 where

import Data.Set(fromList,size)

import AOC

aoc = AOC {
    day="../../2022/input/06",
    aocTests=[
        AOCTest {
            testData="mjqjpqmgbljsphdztnvjfqwrcgsmlb",
            testResult=Just "7",
            testResult2=Just "19"
            },
        AOCTest {
            testData="bvwbjplbgvbhsrlpgdmjqwftvncz",
            testResult=Just "5",
            testResult2=Just "23"
            },
        AOCTest {
            testData="nppdvjthqldpwncqszvftbrmjlhg",
            testResult=Just "6",
            testResult2=Just "23"
            },
        AOCTest {
            testData="nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
            testResult=Just "10",
            testResult2=Just "29"
            },
        AOCTest {
            testData="zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",
            testResult=Just "11",
            testResult2=Just "26"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 4,
        codeTest2=result 14,
        codeResult=result 4,
        codeResult2=result 14
        }
    }

parse = zip [1..]

result n input@((i,_):_)
  | size (fromList $ map snd $ take n input) == n = i + n - 1
  | otherwise = result n $ tail input
