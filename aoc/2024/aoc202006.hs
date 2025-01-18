module AOC202006 where

import Data.Set(fromList,intersection,size,unions)

import AOC

aoc = AOC {
    day="../../2020/input/06",
    aocTests=[
        AOCTest {
            testData=unlines [
                "abc",
                "",
                "a",
                "b",
                "c",
                "",
                "ab",
                "ac",
                "",
                "a",
                "a",
                "a",
                "a",
                "",
                "b"
                ],
            testResult=Just "11",
            testResult2=Just "6"
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

parse = map (map fromList) . parseBlankLineSeparated

result = sum . map (size . unions)

result2 = sum . map (size . intersections)
  where intersections (s:ss) = foldr intersection s ss
