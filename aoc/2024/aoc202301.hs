module AOC202301 where

import Data.Char(isDigit)

import AOC

aoc = AOC {
    day="../../2023/input/01",
    aocTests=[
        AOCTest {
            testData=unlines [
                "1abc2",
                "pqr3stu8vwx",
                "a1b2c3d4e5f",
                "treb7uchet"
                ],
            testResult=Just "142",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "two1nine",
                "eightwothree",
                "abcone2threexyz",
                "xtwone3four",
                "4nineeightseven2",
                "zoneight234",
                "7pqrstsixteen"
                ],
            testResult=Nothing,
            testResult2=Just "281"
            }
        ],
    aocCode=Code {
        codeParse=lines,
        codeParse2=map parse2 . lines,
        codeTest=result,
        codeTest2=result,
        codeResult=result,
        codeResult2=result
        }
    }

result :: [String] -> Int
result = sum . map (calibration . filter isDigit)
  where calibration digits = read [head digits,last digits]

parse2 [] = []
parse2 ('o':'n':rest@('e':_)) = '1':parse2 rest
parse2 ('t':'w':rest@('o':_)) = '2':parse2 rest
parse2 ('t':'h':'r':'e':rest@('e':_)) = '3':parse2 rest
parse2 ('f':'o':'u':'r':rest) = '4':parse2 rest
parse2 ('f':'i':'v':rest@('e':_)) = '5':parse2 rest
parse2 ('s':'i':'x':rest) = '6':parse2 rest
parse2 ('s':'e':'v':'e':rest@('n':_)) = '7':parse2 rest
parse2 ('e':'i':'g':'h':rest@('t':_)) = '8':parse2 rest
parse2 ('n':'i':'n':rest@('e':_)) = '9':parse2 rest
parse2 (c:rest)
  | isDigit c = c:parse2 rest
  | otherwise = parse2 rest
