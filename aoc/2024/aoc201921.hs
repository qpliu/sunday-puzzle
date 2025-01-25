module AOC201921 where

import Data.Char(ord)

import AOC
import AOC2019

aoc = AOC {
    day="../../2019/input/21",
    aocTests=[],
    aocCode=Code {
        codeParse=parseIntCode,
        codeParse2=parseIntCode,
        codeTest=undefined,
        codeTest2=undefined,
        codeResult=result,
        codeResult2=result2
        }
    }

-- There are 16 possible sensor results.  If A is false, always jump,
-- so there are 8 possible sensor results that remain, making 256
-- possible programs.
result = last . unsafeIntCode (map ord script)
  where script = unlines [
          "NOT C J",
          "OR D T",
          "AND D J",
          "NOT A T",
          "OR T J",
          "WALK"
          ]

result2 = last . unsafeIntCode (map ord script)
  where script = unlines [
          "OR D J",
          "OR E T",
          "OR H T",
          "AND T J",
          "NOT B T",
          "NOT T T",
          "AND C T",
          "NOT T T",
          "AND T J",
          "NOT A T",
          "OR T J",
          "RUN"
          ]
