module AOC202309 where

import AOC

aoc = AOC {
    day="../../2023/input/09",
    aocTests=[
        AOCTest {
            testData=unlines [
                "0 3 6 9 12 15",
                "1 3 6 10 15 21",
                "10 13 16 21 30 45"
                ],
            testResult=Just "114",
            testResult2=Just "2"
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

parse = map parseInts . lines

next ns
  | all (== 0) ns = 0
  | otherwise = last ns + next (zipWith (-) (drop 1 ns) ns)

result = sum . map next

result2 = sum . map (next . reverse)
