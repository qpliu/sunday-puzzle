{-
--- Day 16: Chronal Classification ---

As you see the Elves defend their hot chocolate successfully, you go back to
falling through time. This is going to become a problem.

If you're ever going to return to your own time, you need to understand how
this device on your wrist works. You have a little while before you reach your
next destination, and with a bit of trial and error, you manage to pull up a
programming manual on the device's tiny screen.

According to the manual, the device has four registers (numbered 0 through 3)
that can be manipulated by instructions containing one of 16 opcodes. The
registers start with the value 0.

Every instruction consists of four values: an opcode, two inputs (named A and
B), and an output (named C), in that order. The opcode specifies the behavior
of the instruction and how the inputs are interpreted. The output, C, is always
treated as a register.

In the opcode descriptions below, if something says "value A", it means to take
the number given as A literally. (This is also called an "immediate" value.) If
something says "register A", it means to use the number given as A to read from
(or write to) the register with that number. So, if the opcode addi adds register A and value B, storing the result in register C, and the instruction addi 0
7 3 is encountered, it would add 7 to the value contained by register 0 and
store the sum in register 3, never modifying registers 0, 1, or 2 in the
process.

Many opcodes are similar except for how they interpret their arguments. The
opcodes fall into seven general categories:

Addition:

 - addr (add register) stores into register C the result of adding register A
   and register B.
 - addi (add immediate) stores into register C the result of adding register A
   and value B.

Multiplication:

 - mulr (multiply register) stores into register C the result of multiplying
   register A and register B.
 - muli (multiply immediate) stores into register C the result of multiplying
   register A and value B.

Bitwise AND:

 - banr (bitwise AND register) stores into register C the result of the bitwise
   AND of register A and register B.
 - bani (bitwise AND immediate) stores into register C the result of the
   bitwise AND of register A and value B.

Bitwise OR:

 - borr (bitwise OR register) stores into register C the result of the bitwise
   OR of register A and register B.
 - bori (bitwise OR immediate) stores into register C the result of the bitwise
   OR of register A and value B.

Assignment:

 - setr (set register) copies the contents of register A into register C.
   (Input B is ignored.)
 - seti (set immediate) stores value A into register C. (Input B is ignored.)

Greater-than testing:

 - gtir (greater-than immediate/register) sets register C to 1 if value A is
   greater than register B. Otherwise, register C is set to 0.
 - gtri (greater-than register/immediate) sets register C to 1 if register A is
   greater than value B. Otherwise, register C is set to 0.
 - gtrr (greater-than register/register) sets register C to 1 if register A is
   greater than register B. Otherwise, register C is set to 0.

Equality testing:

 - eqir (equal immediate/register) sets register C to 1 if value A is equal to
   register B. Otherwise, register C is set to 0.
 - eqri (equal register/immediate) sets register C to 1 if register A is equal
   to value B. Otherwise, register C is set to 0.
 - eqrr (equal register/register) sets register C to 1 if register A is equal
   to register B. Otherwise, register C is set to 0.

Unfortunately, while the manual gives the name of each opcode, it doesn't seem
to indicate the number. However, you can monitor the CPU to see the contents of
the registers before and after instructions are executed to try to work them
out. Each opcode has a number from 0 through 15, but the manual doesn't say
which is which. For example, suppose you capture the following sample:

| Before: [3, 2, 1, 1]
| 9 2 1 2
| After:  [3, 2, 2, 1]

This sample shows the effect of the instruction 9 2 1 2 on the registers.
Before the instruction is executed, register 0 has value 3, register 1 has
value 2, and registers 2 and 3 have value 1. After the instruction is executed,
register 2's value becomes 2.

The instruction itself, 9 2 1 2, means that opcode 9 was executed with A=2,
B=1, and C=2. Opcode 9 could be any of the 16 opcodes listed above, but only
three of them behave in a way that would cause the result shown in the sample:

 - Opcode 9 could be mulr: register 2 (which has a value of 1) times register 1
   (which has a value of 2) produces 2, which matches the value stored in the
   output register, register 2.
 - Opcode 9 could be addi: register 2 (which has a value of 1) plus value 1
   produces 2, which matches the value stored in the output register, register
   2.
 - Opcode 9 could be seti: value 2 matches the value stored in the output
   register, register 2; the number given for B is irrelevant.

None of the other opcodes produce the result captured in the sample. Because of
this, the sample above behaves like three opcodes.

You collect many of these samples (the first section of your puzzle input). The
manual also includes a small test program (the second section of your puzzle
input) - you can ignore it for now.

Ignoring the opcode numbers, how many samples in your puzzle input behave like
three or more opcodes?
-}

import Data.Bits((.&.),(.|.))
import Data.Char(isDigit)
import Data.Map(Map,adjust,fromList,insert,mapWithKey,toList,(!))
import qualified Data.Map
import Data.Set(Set,difference,intersection,unions,size)
import qualified Data.Set

readNum :: String -> Int
readNum = read . filter isDigit

type Sample = ([Int],Int,(Int,Int,Int),[Int])

parse :: String -> ([Sample],[Int])
parse = psamples [] . words
  where
    psamples samples ("Before:":rb1:rb2:rb3:rb4:op:a:b:c:"After:":ra1:ra2:ra3:ra4:rest) =
        psamples (([readNum rb1,readNum rb2,readNum rb3,readNum rb4],readNum op,(readNum a,readNum b,readNum c),[readNum ra1,readNum ra2,readNum ra3,readNum ra4]):samples) rest
    psamples samples rest = (reverse samples,map readNum rest)

type Insn = Map Int Int -> (Int,Int,Int) -> Map Int Int

insns :: [(String,Insn)]
insns = [
    ("addr",\ regs (a,b,c) -> insert c (regs!a + regs!b) regs),
    ("addi",\ regs (a,b,c) -> insert c (regs!a + b) regs),
    ("mulr",\ regs (a,b,c) -> insert c (regs!a * regs!b) regs),
    ("muli",\ regs (a,b,c) -> insert c (regs!a * b) regs),
    ("banr",\ regs (a,b,c) -> insert c (regs!a .&. regs!b) regs),
    ("bani",\ regs (a,b,c) -> insert c (regs!a .&. b) regs),
    ("borr",\ regs (a,b,c) -> insert c (regs!a .|. regs!b) regs),
    ("bori",\ regs (a,b,c) -> insert c (regs!a .|. b) regs),
    ("setr",\ regs (a,b,c) -> insert c (regs!a) regs),
    ("seti",\ regs (a,b,c) -> insert c a regs),
    ("gtir",\ regs (a,b,c) -> insert c (if a > regs!b then 1 else 0) regs),
    ("gtri",\ regs (a,b,c) -> insert c (if regs!a > b then 1 else 0) regs),
    ("gtrr",\ regs (a,b,c) -> insert c (if regs!a > regs!b then 1 else 0) regs),
    ("eqir",\ regs (a,b,c) -> insert c (if a == regs!b then 1 else 0) regs),
    ("eqri",\ regs (a,b,c) -> insert c (if regs!a == b then 1 else 0) regs),
    ("eqrr",\ regs (a,b,c) -> insert c (if regs!a == regs!b then 1 else 0) regs)
    ]

match :: Sample -> Insn -> Bool
match (before,_,operands,after) insn =
    insn (fromList (zip [0..] before)) operands == (fromList (zip [0..] after))

testData :: String
testData = "Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]"

matches3 :: Sample -> Bool
matches3 sample = length (filter (match sample . snd) insns) >= 3

test :: ()
test
  | map fst (filter (match (head $ fst $ parse testData) . snd) insns) /= ["addi","mulr","seti"] = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . filter matches3 . fst . parse) $ readFile "input/16.txt"

initScan :: Map Int (Set String)
initScan = fromList $ zip [0..15] $ repeat allInsns
  where allInsns = Data.Set.fromList (map fst insns)

scanSample :: Sample -> Map Int (Set String) -> Map Int (Set String)
scanSample sample@(_,op,_,_) m =
    adjust (intersection matchedInsns) op m
  where
    matchedInsns = Data.Set.fromList (map fst $ filter (match sample . snd) insns)

propogateKnowns :: Map Int (Set String) -> Map Int String
propogateKnowns m
  | hasUnknown = propogateKnowns $ mapWithKey removeSingles m
  | otherwise = Data.Map.map minimum m
  where
    hasUnknown = any ((> 1) . size) m
    removeSingles op insnNames = insnNames `difference` singles op
    singles op = unions [insnName | (op2,insnName) <- toList m, op2 /= op, size insnName == 1]

toInsnTable :: Map Int String -> Map Int Insn
toInsnTable = Data.Map.map ((fromList insns)!)

initRegs :: Map Int Int
initRegs = fromList $ zip [0..3] $ repeat 0

interp :: Map Int Insn -> Map Int Int -> [Int] -> Map Int Int
interp insnTable regs (op:a:b:c:rest) = interp insnTable ((insnTable!op) regs (a,b,c)) rest
interp insnTable regs _ = regs

run2 :: String -> Map Int Int
run2 input = interp insnTable initRegs code
  where
    (samples,code) = parse input
    insnTable = toInsnTable $ propogateKnowns $ foldr scanSample initScan samples

part2 :: IO Int
part2 = fmap ((!0) . run2) $ readFile "input/16.txt"
