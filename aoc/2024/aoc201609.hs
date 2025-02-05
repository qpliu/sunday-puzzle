module AOC201609 where

import Data.Char(isSpace)

import AOC

aoc = AOC {
    day="../../2016/input/09",
    aocTests=[
        AOCTest {
            testData="ADVENT",
            testResult=Just "6",
            testResult2=Just "6"
            },
        AOCTest {
            testData="A(1x5)BC",
            testResult=Just "7",
            testResult2=Just "7"
            },
        AOCTest {
            testData="(3x3)XYZ",
            testResult=Just "9",
            testResult2=Just "9"
            },
        AOCTest {
            testData="A(2x2)BCD(2x2)EFG",
            testResult=Just "11",
            testResult2=Just "11"
            },
        AOCTest {
            testData="(6x1)(1x3)A",
            testResult=Just "6",
            testResult2=Just "3"
            },
        AOCTest {
            testData="X(8x2)(3x3)ABCY",
            testResult=Just "18",
            testResult2=Just "20"
            },
        AOCTest {
            testData="(27x12)(20x12)(13x14)(7x10)(1x12)A",
            testResult=Nothing,
            testResult2=Just "241920"
            },
        AOCTest {
            testData="(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN",
            testResult=Nothing,
            testResult2=Just "445"
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

parse = filter (not . isSpace)

decompressed :: Bool -> String -> Int
decompressed recurse str
  | null marker = length str
  | recurse =
      length h + (decompressed True (take len rest))*count
               + decompressed recurse (drop len rest)
  | otherwise = length h + len*count + decompressed False (drop len rest)
  where
    (h,marker) = span (/= '(') str
    (m,')':rest) = span (/= ')') marker
    [len,count] = parseInts m

result = decompressed False

result2 = decompressed True
