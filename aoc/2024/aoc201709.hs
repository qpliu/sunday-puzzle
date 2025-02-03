module AOC201709 where

import AOC

aoc = AOC {
    day="../../2017/input/09",
    aocTests=[
        AOCTest {
            testData="{}",
            testResult=Just "1",
            testResult2=Nothing
            },
        AOCTest {
            testData="{{{}}}",
            testResult=Just "6",
            testResult2=Nothing
            },
        AOCTest {
            testData="{{},{}}",
            testResult=Just "5",
            testResult2=Nothing
            },
        AOCTest {
            testData="{{{},{},{{}}}}",
            testResult=Just "16",
            testResult2=Nothing
            },
        AOCTest {
            testData="{<a>,<a>,<a>,<a>}",
            testResult=Just "1",
            testResult2=Nothing
            },
        AOCTest {
            testData="{{<ab>},{<ab>},{<ab>},{<ab>}}",
            testResult=Just "9",
            testResult2=Nothing
            },
        AOCTest {
            testData="{{<!!>},{<!!>},{<!!>},{<!!>}}",
            testResult=Just "9",
            testResult2=Nothing
            },
        AOCTest {
            testData="{{<a!>},{<a!>},{<a!>},{<ab>}}",
            testResult=Just "3",
            testResult2=Nothing
            },
        AOCTest {
            testData="<>",
            testResult=Nothing,
            testResult2=Just "0"
            },
        AOCTest {
            testData="<random characters>",
            testResult=Nothing,
            testResult2=Just "17"
            },
        AOCTest {
            testData="<<<<>",
            testResult=Nothing,
            testResult2=Just "3"
            },
        AOCTest {
            testData="<{!>}>",
            testResult=Nothing,
            testResult2=Just "2"
            },
        AOCTest {
            testData="<!!>",
            testResult=Nothing,
            testResult2=Just "0"
            },
        AOCTest {
            testData="<!!!>>",
            testResult=Nothing,
            testResult2=Just "0"
            },
        AOCTest {
            testData="<{o\"i!a,<{i<a>",
            testResult=Nothing,
            testResult2=Just "10"
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

parse = id

score :: Int -> Int -> Bool -> String -> Int
score total group garbage [] = total
score total group True ('!':_:rest) = score total group True rest
score total group True ('>':rest) = score total group False rest
score total group True (_:rest) = score total group True rest
score total group False ('<':rest) = score total group True rest
score total group False ('{':rest) = score (total+group) (group+1) False rest
score total group False ('}':rest) = score total (group-1) False rest
score total group False (_:rest) = score total group False rest

result = score 0 1 False

count :: Bool -> Int -> String -> Int
count garbage n [] = n
count True n ('!':_:rest) = count True n rest
count True n ('>':rest) = count False n rest
count True n (_:rest) = count True (n+1) rest
count False n ('<':rest) = count True n rest
count False n (_:rest) = count False n rest

result2 = count False 0
