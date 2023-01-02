{-
--- Day 18: Duet ---

You discover a tablet containing some strange assembly code labeled simply
"Duet". Rather than bother the sound card with it, you decide to run the code
yourself. Unfortunately, you don't see any documentation, so you're left to
figure out what the instructions mean on your own.

It seems like the assembly is meant to operate on a set of registers that are
each named with a single letter and that can each hold a single integer. You
suppose each register should start with a value of 0.

There aren't that many instructions, so it shouldn't be hard to figure out what
they do. Here's what you determine:

 - snd X plays a sound with a frequency equal to the value of X.
 - set X Y sets register X to the value of Y.
 - add X Y increases register X by the value of Y.
 - mul X Y sets register X to the result of multiplying the value contained in
   register X by the value of Y.
 - mod X Y sets register X to the remainder of dividing the value contained in
   register X by the value of Y (that is, it sets X to the result of X modulo
   Y).
 - rcv X recovers the frequency of the last sound played, but only when the
   value of X is not zero. (If it is zero, the command does nothing.)
 - jgz X Y jumps with an offset of the value of Y, but only if the value of X
   is greater than zero. (An offset of 2 skips the next instruction, an offset
   of -1 jumps to the previous instruction, and so on.)

Many of the instructions can take either a register (a single letter) or a
number. The value of a register is the integer it contains; the value of a
number is that number.

After each jump instruction, the program continues with the instruction to
which the jump jumped. After any other instruction, the program continues with
the next instruction. Continuing (or jumping) off either end of the program
terminates it.

For example:

| set a 1
| add a 2
| mul a a
| mod a 5
| snd a
| set a 0
| rcv a
| jgz a -1
| set a 1
| jgz a -2

 - The first four instructions set a to 1, add 2 to it, square it, and then set
   it to itself modulo 5, resulting in a value of 4.
 - Then, a sound with frequency 4 (the value of a) is played.
 - After that, a is set to 0, causing the subsequent rcv and jgz instructions
   to both be skipped (rcv because a is 0, and jgz because a is not greater
   than 0).
 - Finally, a is set to 1, causing the next jgz instruction to activate,
   jumping back two instructions to another jump, which jumps again to the rcv,
   which ultimately triggers the recover operation.

At the time the recover operation is executed, the frequency of the last sound
played is 4.

What is the value of the recovered frequency (the value of the most recently
played sound) the first time a rcv instruction is executed with a non-zero
value?
-}

import Data.Char(isDigit)
import Data.Map(Map,empty,findWithDefault,insert)
import qualified Data.Map

val :: Map String Int -> String -> Int
val regs token
  | head token == '-' || isDigit (head token) = read token
  | otherwise = findWithDefault 0 token regs

interp :: ([Int],Map String Int) -> ([[String]],[[String]]) -> ([Int],Map String Int)
interp (recovered,regs) ([],_) = (recovered,regs)
interp (recovered,regs) (insn:forward,backward) = interp1 insn
  where
    v = val regs
    interp1 ("snd":x:_) = interp (recovered,insert "snd" (v x) regs) (forward,insn:backward)
    interp1 ("set":x:y:_) = interp (recovered,insert x (v y) regs) (forward,insn:backward)
    interp1 ("add":x:y:_) = interp (recovered,insert x (v x + v y) regs) (forward,insn:backward)
    interp1 ("mul":x:y:_) = interp (recovered,insert x (v x * v y) regs) (forward,insn:backward)
    interp1 ("mod":x:y:_) = interp (recovered,insert x (v x `mod` v y) regs) (forward,insn:backward)
    interp1 ("rcv":x:_)
      | v x == 0 = interp (recovered,regs) (forward,insn:backward)
      | otherwise = (v "snd":recovered,regs)
    interp1 ("jgz":x:y:_)
      | v x <= 0 = interp (recovered,regs) (forward,insn:backward)
      | v y >= 0 = interp (recovered,regs) (nextForward,reverse skipForward ++ backward)
      | (-(v y)) >= length backward = (recovered,regs)
      | otherwise = interp (recovered,regs) (reverse skipBackward++insn:forward,nextBackward)
      where (skipForward,nextForward) = splitAt (v y) (insn:forward)
            (skipBackward,nextBackward) = splitAt (-(v y)) backward
    interp1 _ = error (show insn)

parse :: String -> ([[String]],[[String]])
parse s = (map words $ lines s,[])

testData :: String
testData = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"

test :: ()
test
  | fst (interp ([],empty) (parse testData)) /= [4] = error "a"
  | otherwise = ()

part1 :: IO [Int]
part1 = fmap (fst . interp ([],empty) . parse) $ readFile "input/18.txt"

type State = ([Int],[Int],Map String Int,[[String]],[[String]])

interp2 :: State -> State
interp2 (rcv,snd,regs,[],_) = (rcv,snd,regs,[],[])
interp2 (rcv,snd,regs,insn:forward,backward) = interp1 insn
  where
    v = val regs
    interp1 ("snd":x:_) = interp2 (rcv,(v x):snd,regs,forward,insn:backward)
    interp1 ("set":x:y:_) = interp2 (rcv,snd,insert x (v y) regs,forward,insn:backward)
    interp1 ("add":x:y:_) = interp2 (rcv,snd,insert x (v x + v y) regs,forward,insn:backward)
    interp1 ("mul":x:y:_) = interp2 (rcv,snd,insert x (v x * v y) regs,forward,insn:backward)
    interp1 ("mod":x:y:_) = interp2 (rcv,snd,insert x (v x `mod` v y) regs,forward,insn:backward)
    interp1 ("rcv":x:_)
      | null rcv = (rcv,snd,regs,insn:forward,backward)
      | otherwise = interp2 (tail rcv,snd,insert x (head rcv) regs,forward,insn:backward)
    interp1 ("jgz":x:y:_)
      | v x <= 0 = interp2 (rcv,snd,regs,forward,insn:backward)
      | v y >= 0 = interp2 (rcv,snd,regs,nextForward,reverse skipForward ++ backward)
      | (-(v y)) >= length backward = (rcv,snd,regs,[],[])
      | otherwise = interp2 (rcv,snd,regs,reverse skipBackward++insn:forward,nextBackward)
      where (skipForward,nextForward) = splitAt (v y) (insn:forward)
            (skipBackward,nextBackward) = splitAt (-(v y)) backward
    interp1 _ = error (show insn)

init2 :: String -> (Int,(State,State))
init2 s = (0,(([],[],empty,insns,[]),([],[],insert "p" 1 empty,insns,[])))
  where insns = map words $ lines s

step2 :: (Int,(State,State)) -> (Int,(State,State))
step2 (sendCount,(state0@(rcv0,snd0,regs0,insns0,back0),state1@(rcv1,snd1,regs1,insns1,back1)))
  | not (null snd0) || not (null snd1) =
    (sendCount+length snd1,((rcv0++reverse snd1,[],regs0,insns0,back0),(rcv1++reverse snd0,[],regs1,insns1,back1)))
  | otherwise = (sendCount,(interp2 state0,interp2 state1))

terminated :: (Int,(State,State)) -> Bool
terminated (_,((rcv0,snd0,_,insns0,_),(rcv1,snd1,_,insns1,_))) =
    null snd0 && null snd1 && (null insns0 || (null rcv0 && (take 1 (head insns0) == ["rcv"]))) && (null insns1 || (null rcv1 && (take 1 (head insns1) == ["rcv"])))

run2 :: (Int,(State,State)) -> (Int,(State,State))
run2 state
  | terminated state = state
  | otherwise = run2 (step2 state)

testData2 :: String
testData2 = "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d"

test2 :: ()
test2
  | fst (run2 $ init2 testData2) /= 3 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (fst . run2 . init2) $ readFile "input/18.txt"
