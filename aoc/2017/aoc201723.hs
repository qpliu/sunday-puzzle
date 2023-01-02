{-
--- Day 23: Coprocessor Conflagration ---

You decide to head directly to the CPU and fix the printer from there. As you
get close, you find an experimental coprocessor doing so much work that the
local programs are afraid it will halt and catch fire. This would cause serious
issues for the rest of the computer, so you head in and see what you can do.

The code it's running seems to be a variant of the kind you saw recently on
that tablet. The general functionality seems very similar, but some of the
instructions are different:

 - set X Y sets register X to the value of Y.
 - sub X Y decreases register X by the value of Y.
 - mul X Y sets register X to the result of multiplying the value contained in
   register X by the value of Y.
 - jnz X Y jumps with an offset of the value of Y, but only if the value of X
   is not zero. (An offset of 2 skips the next instruction, an offset of -1
   jumps to the previous instruction, and so on.)

Only the instructions listed above are used. The eight registers here, named a
through h, all start at 0.

The coprocessor is currently set to some kind of debug mode, which allows for
testing, but prevents it from doing any meaningful work.

If you run the program (your puzzle input), how many times is the mul
instruction invoked?
-}

import Data.Char(isDigit)
import Data.Map(Map,empty,findWithDefault,insert)
import qualified Data.Map

val :: Map String Int -> String -> Int
val regs token
  | head token == '-' || isDigit (head token) = read token
  | otherwise = findWithDefault 0 token regs

interp :: (Int,Map String Int) -> ([[String]],[[String]]) -> (Int,Map String Int)
interp (mulCount,regs) ([],_) = (mulCount,regs)
interp (mulCount,regs) (insn:forward,backward) = interp1 insn
  where
    v = val regs
    interp1 ("set":x:y:_) = interp (mulCount,insert x (v y) regs) (forward,insn:backward)
    interp1 ("sub":x:y:_) = interp (mulCount,insert x (v x - v y) regs) (forward,insn:backward)
    interp1 ("mul":x:y:_) = interp (mulCount+1,insert x (v x * v y) regs) (forward,insn:backward)
    interp1 ("jnz":x:y:_)
      | v x == 0 = interp (mulCount,regs) (forward,insn:backward)
      | v y >= 0 = interp (mulCount,regs) (nextForward,reverse skipForward ++ backward)
      | (-(v y)) >= length backward = (mulCount,regs)
      | otherwise = interp (mulCount,regs) (reverse skipBackward++insn:forward,nextBackward)
      where (skipForward,nextForward) = splitAt (v y) (insn:forward)
            (skipBackward,nextBackward) = splitAt (-(v y)) backward
    interp1 _ = error (show insn)

test :: ()
test
  | otherwise = ()

part1 :: IO Int
part1 = fmap (fst . interp (0,empty) . flip (,) [] . map words . lines) $ readFile "input/23.txt"

-- Decompiling my input,
-- if a = 0, h is the number of non-prime numbers in the range 93-93, or 1.

-- if a is not 0, h is number of non-prime numbers in
-- 109300, 109300+17, 109300+2*17, ... 109300+1000*17.

notPrime :: Int -> Bool
notPrime n = or [n `mod` x == 0 | x <- [2..n-1]]

part2 :: Int
part2 = length $ filter notPrime $ [109300,109300+17 .. 109300+17*1000]
