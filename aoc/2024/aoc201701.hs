module AOC201701 where

import Data.Char(isDigit,ord)

import AOC

aoc = AOC {
    day="../../2017/input/01",
    aocTests=[
        AOCTest {
            testData="1122",
            testResult=Just "3",
            testResult2=Nothing
            },
        AOCTest {
            testData="1111",
            testResult=Just "4",
            testResult2=Nothing
            },
        AOCTest {
            testData="1234",
            testResult=Just "0",
            testResult2=Nothing
            },
        AOCTest {
            testData="91212129",
            testResult=Just "9",
            testResult2=Nothing
            },
        AOCTest {
            testData="1212",
            testResult=Nothing,
            testResult2=Just "6"
            },
        AOCTest {
            testData="1221",
            testResult=Nothing,
            testResult2=Just "0"
            },
        AOCTest {
            testData="123425",
            testResult=Nothing,
            testResult2=Just "4"
            },
        AOCTest {
            testData="123123",
            testResult=Nothing,
            testResult2=Just "12"
            },
        AOCTest {
            testData="12131415",
            testResult=Nothing,
            testResult2=Just "4"
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

parse = map ((+ (-ord '0')) . ord) . filter isDigit

result digits =
    sum $ map fst $ filter (uncurry (==)) $ zip (last digits:digits) digits

result2 digits = (*2) $ sum $ map fst $ filter (uncurry (==))
                      $ zip digits $ drop (length digits `div` 2) digits
