module AOC201508 where

import AOC

aoc = AOC {
    day="../../2015/input/08",
    aocTests=[
        AOCTest {
            testData=unlines [
                "\"\"",
                "\"abc\"",
                "\"aaa\\\"aaa\"",
                "\"\\x27\""
                ],
            testResult=Just "12",
            testResult2=Just "19"
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

parse = lines

counts :: Int -> String -> Int
counts n ('\\':'x':_:_:rest) = counts (n+3) rest
counts n ('\\':_:rest) = counts (n+1) rest
counts n (_:rest) = counts n rest
counts n [] = n

result = sum . map (counts 2)

counts2 :: Int -> String -> Int
counts2 n ('\\':rest) = counts2 (n+1) rest
counts2 n ('"':rest) = counts2 (n+1) rest
counts2 n (_:rest) = counts2 n rest
counts2 n [] = n

result2 = sum . map (counts2 2)
