module AOC202402 where

import AOC

aoc = AOC {
    day="02",
    aocTests=[
        AOCTest {
            testData=unlines [
                "7 6 4 2 1",
                "1 2 7 8 9",
                "9 7 6 2 1",
                "1 3 2 4 5",
                "8 6 4 4 1",
                "1 3 6 7 9"
                ],
            testResult=Just "2",
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

parse = map (map read . words) . lines

result = length . filter safe

safe r = all (`elem` [1,2,3]) diffs || all (`elem` [-1,-2,-3]) diffs
  where diffs = zipWith (-) r (tail r)

result2 = length . filter dampedSafe

dampedSafe r = safe r || any (safe . rm) [0..length r-1]
  where rm i = take i r ++ drop (i+1) r
