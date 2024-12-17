module AOC202417 where

import Data.Array(listArray,(!))
import Data.Bits(shiftR,xor)
import Data.List(nub)

import AOC

aoc = AOC {
    day="17",
    testData=unlines [
    "Register A: 729",
    "Register B: 0",
    "Register C: 0",
    "",
    "Program: 0,1,5,4,3,0"
    ],
    testResult="[4,6,3,5,6,3,5,2,1,0]",
    testData2=unlines [
    "Register A: 2024",
    "Register B: 0",
    "Register C: 0",
    "",
    "Program: 0,3,5,4,3,0"
    ],
    testResult2="117440",
    aocParse=parseInts,
    aocTest=result,
    aocResult=result,
    aocParse2=parseInts,
    aocTest2=result2,
    aocResult2=result2
    }

result (a:b:c:rest) =
    run1 (length rest) (listArray (0,length rest) rest) a b c 0

run1 len code a b c pc
  | pc+1 >= len = []
  | otherwise = insn (code!pc) (code!(pc+1))
  where
    r = run1 len code
    combo 0 = 0
    combo 1 = 1
    combo 2 = 2
    combo 3 = 3
    combo 4 = a
    combo 5 = b
    combo 6 = c
    insn 0 arg = r (a `shiftR` combo arg) b c (pc+2)
    insn 1 arg = r a (b `xor` arg) c (pc+2)
    insn 2 arg = r a (combo arg `mod` 8) c (pc+2)
    insn 3 arg
      | a == 0 = r a b c (pc+2)
      | otherwise = r a b c (combo arg)
    insn 4 arg = r a (b `xor` c) c (pc+2)
    insn 5 arg = (combo arg `mod` 8) : r a b c (pc+2)
    insn 6 arg = r a (a `shiftR` combo arg) c (pc+2)
    insn 7 arg = r a b (a `shiftR` combo arg) (pc+2)

-- disassembing:
-- 0:
--  do stuff without updating A
--  output does not depend on the initial values of B and C
--  A <- A/8
--  if A /= 0, goto 0

-- find A 3 bits at a time
findA input output
  | length output == 1 = find1 0
  | otherwise = nub $ concat [find1 (a*8) | a <- findA input (tail output)]
  where
    find1 a = [a+i | i <- [0..7], result (a+i:input) == output]

result2 (_:input@(_:_:output)) = minimum $ findA input output
