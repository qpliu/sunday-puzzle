module AOC202202 where

import AOC

aoc = AOC {
    day="../../2022/input/02",
    aocTests=[
        AOCTest {
            testData=unlines [
                "A Y",
                "B X",
                "C Z"
                ],
            testResult=Just "15",
            testResult2=Just "12"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse = map words . lines

score :: [String] -> Int
score ["A","X"] = 1 + 3
score ["A","Y"] = 2 + 6
score ["A","Z"] = 3 + 0
score ["B","X"] = 1 + 0
score ["B","Y"] = 2 + 3
score ["B","Z"] = 3 + 6
score ["C","X"] = 1 + 6
score ["C","Y"] = 2 + 0
score ["C","Z"] = 3 + 3

result = sum . map score

score2 :: [String] -> Int
score2 ["A","X"] = 3 + 0
score2 ["A","Y"] = 1 + 3
score2 ["A","Z"] = 2 + 6
score2 ["B","X"] = 1 + 0
score2 ["B","Y"] = 2 + 3
score2 ["B","Z"] = 3 + 6
score2 ["C","X"] = 2 + 0
score2 ["C","Y"] = 3 + 3
score2 ["C","Z"] = 1 + 6

result2 = sum . map score2
