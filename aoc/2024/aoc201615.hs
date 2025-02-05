module AOC201615 where

import AOC

aoc = AOC {
    day="../../2016/input/15",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Disc #1 has 5 positions; at time=0, it is at position 4.",
                "Disc #2 has 2 positions; at time=0, it is at position 1."
                ],
            testResult=Just "5",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result,
        codeResult=result,
        codeResult2=result
        }
    }

parse = map (toRecurrence . parseInts) . lines
  where toRecurrence [t1,recur,0,s0] = ((-t1-s0) `mod` recur,recur)

parse2 = add11 . parse
  where
    add11 discs = ((-t1) `mod` 11,11):discs
      where t1 = length discs + 1

result = fst . foldr convergences (1,1)
