module AOC202512 where

import Data.Bits(popCount)
import Data.List(nub,transpose)

import AOC

aoc = AOC {
    day="12",
    aocTests=[
        AOCTest {
            testData=unlines [
                "0:",
                "###",
                "##.",
                "##.",
                "",
                "1:",
                "###",
                "##.",
                ".##",
                "",
                "2:",
                ".##",
                "###",
                "##.",
                "",
                "3:",
                "##.",
                "###",
                "##.",
                "",
                "4:",
                "###",
                "#..",
                "###",
                "",
                "5:",
                "###",
                ".#.",
                "###",
                "",
                "4x4: 0 0 0 0 2 0",
                "12x5: 1 0 1 0 2 2",
                "12x5: 1 0 1 0 3 2"
            ],
            testResult=Nothing,
            testResult2=Nothing
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

parse :: String -> [((Int,Int),[([(Integer,Integer,Integer)],Int)])]
parse = p . parseBlankLineSeparated
  where
    p inputs = map (parseRegion . parseInts) (last inputs)
      where
        shapes = map (parseShape . tail) (init inputs)
        parseRegion (w:h:counts) = ((w,h),zip shapes counts)

    parseShape spec =
        map toBits (nub [spec, transpose spec,
                         reverse spec, transpose (reverse spec),
                         map reverse spec, transpose (map reverse spec),
                         reverse (map reverse spec),
                         transpose (reverse (map reverse spec))])
    toBits [a,b,c] = (toBitRow a,toBitRow b,toBitRow c)
    toBitRow row = sum [if ch == '#' then b else 0 | (ch,b) <- zip row [1,2,4]]

result = length . filter fits

fits ((h,w),presents)
  -- every shape fits in 3x3
  | (h `div` 3)*(w `div` 3) >= sum (map snd presents) = True
  | h*w < sum (map cellCount presents) = False
  | otherwise = error "can't tell without packing"
  where
    cellCount (((a,b,c):_),n) = n*(popCount a + popCount b + popCount c)

result2 = const 0
