module AOC202402 where

import AOC

aoc = AOC {
    day="02",
    testData=unlines [
    "7 6 4 2 1",
    "1 2 7 8 9",
    "9 7 6 2 1",
    "1 3 2 4 5",
    "8 6 4 4 1",
    "1 3 6 7 9"
    ],
    testResult="2",
    testData2="",
    testResult2="4",
    aocParse=parse,
    aocTest=result,
    aocResult=result,
    aocParse2=parse,
    aocTest2=result2,
    aocResult2=result2
    }

parse = map (map read . words) . lines

result = length . filter safe

safe r = all (`elem` [1,2,3]) diffs || all (`elem` [-1,-2,-3]) diffs
  where diffs = zipWith (-) r (tail r)

result2 = length . filter dampedSafe

dampedSafe r = safe r || any (safe . rm) [0..length r-1]
  where rm i = take i r ++ drop (i+1) r
