{-
--- Day 19: Go With The Flow ---

With the Elves well on their way constructing the North Pole base, you turn
your attention back to understanding the inner workings of programming the
device.

You can't help but notice that the device's opcodes don't contain any flow
control like jump instructions. The device's manual goes on to explain:

"In programs where flow control is required, the instruction pointer can be
bound to a register so that it can be manipulated directly. This way,
setr/seti can function as absolute jumps, addr/addi can function as relative
jumps, and other opcodes can cause truly fascinating effects."

This mechanism is achieved through a declaration like #ip 1, which would modify
register 1 so that accesses to it let the program indirectly access the
instruction pointer itself. To compensate for this kind of binding, there are
now six registers (numbered 0 through 5); the five not bound to the instruction
pointer behave as normal. Otherwise, the same rules apply as the last time you
worked with this device.

When the instruction pointer is bound to a register, its value is written to
that register just before each instruction is executed, and the value of that
register is written back to the instruction pointer immediately after each
instruction finishes execution. Afterward, move to the next instruction by
adding one to the instruction pointer, even if the value in the instruction
pointer was just updated by an instruction. (Because of this, instructions must
effectively set the instruction pointer to the instruction before the one they
want executed next.)

The instruction pointer is 0 during the first instruction, 1 during the second,
and so on. If the instruction pointer ever causes the device to attempt to load
an instruction outside the instructions defined in the program, the program
instead immediately halts. The instruction pointer starts at 0.

It turns out that this new information is already proving useful: the CPU in
the device is not very powerful, and a background process is occupying most of
its time. You dump the background process' declarations and instructions to a
file (your puzzle input), making sure to use the names of the opcodes rather
than the numbers.

For example, suppose you have the following program:

| #ip 0
| seti 5 0 1
| seti 6 0 2
| addi 0 1 0
| addr 1 2 3
| setr 1 0 0
| seti 8 0 4
| seti 9 0 5

When executed, the following instructions are executed. Each line contains the
value of the instruction pointer at the time the instruction started, the
values of the six registers before executing the instructions (in square
brackets), the instruction itself, and the values of the six registers after
executing the instruction (also in square brackets).

| ip=0 [0, 0, 0, 0, 0, 0] seti 5 0 1 [0, 5, 0, 0, 0, 0]
| ip=1 [1, 5, 0, 0, 0, 0] seti 6 0 2 [1, 5, 6, 0, 0, 0]
| ip=2 [2, 5, 6, 0, 0, 0] addi 0 1 0 [3, 5, 6, 0, 0, 0]
| ip=4 [4, 5, 6, 0, 0, 0] setr 1 0 0 [5, 5, 6, 0, 0, 0]
| ip=6 [6, 5, 6, 0, 0, 0] seti 9 0 5 [6, 5, 6, 0, 0, 9]

In detail, when running this program, the following events occur:

 - The first line (#ip 0) indicates that the instruction pointer should be
   bound to register 0 in this program. This is not an instruction, and so the
   value of the instruction pointer does not change during the processing of
   this line.
 - The instruction pointer contains 0, and so the first instruction is executed
   (seti 5 0 1). It updates register 0 to the current instruction pointer value
   (0), sets register 1 to 5, sets the instruction pointer to the value of
   register 0 (which has no effect, as the instruction did not modify register
   0), and then adds one to the instruction pointer.
 - The instruction pointer contains 1, and so the second instruction, seti 6 0
   2, is executed. This is very similar to the instruction before it: 6 is
   stored in register 2, and the instruction pointer is left with the value 2.
 - The instruction pointer is 2, which points at the instruction addi 0 1 0.
   This is like a relative jump: the value of the instruction pointer, 2, is
   loaded into register 0. Then, addi finds the result of adding the value in
   register 0 and the value 1, storing the result, 3, back in register 0.
   Register 0 is then copied back to the instruction pointer, which will cause
   it to end up 1 larger than it would have otherwise and skip the next
   instruction (addr 1 2 3) entirely. Finally, 1 is added to the instruction
   pointer.
 - The instruction pointer is 4, so the instruction setr 1 0 0 is run. This is
   like an absolute jump: it copies the value contained in register 1, 5, into
   register 0, which causes it to end up in the instruction pointer. The
   instruction pointer is then incremented, leaving it at 6.
 - The instruction pointer is 6, so the instruction seti 9 0 5 stores 9 into
   register 5. The instruction pointer is incremented, causing it to point
   outside the program, and so the program ends.

What value is left in register 0 when the background process halts?
-}

-- I'll assume #ip declarations apply to the instructions that follow until
-- the next #ip declaration.

import Data.Array(Array,bounds,inRange,listArray,(!),(//))
import Data.Bits((.&.),(.|.))
import Data.List(group)

type Insn = (Int,String,(Int,Int,Int))
type State = (Int,Array Int Int)

parse :: String -> Array Int Insn
parse s = listArray (0,length insns - 1) insns
  where
    insns = p (-1) $ words s
    p bip ("#ip":ip:rest) = p (read ip) rest
    p bip (insn:a:b:c:rest) = (bip,insn,(read a,read b,read c)) : p bip rest
    p _ _ = []

initState :: State
initState = (0,listArray (-1,5) [0,0,0,0,0,0,0])

step :: Array Int Insn -> State -> Either State State
step insns state@(ip,regs)
  | not (inRange (bounds insns) ip) = Left state
  | ip == 35 || ip == 26 = Left state
  | otherwise = Right (1+regs2!bip,regs2)
  where
    insn@(bip,op,operands) = insns!ip
    regs1 = regs // [(bip,ip)]
    regs2 = exec op operands regs1

exec :: String -> (Int,Int,Int) -> Array Int Int -> Array Int Int
exec op (a,b,c) regs
  | op == "addr" = regs // [(c,regs!a + regs!b)]
  | op == "addi" = regs // [(c,regs!a + b)]
  | op == "mulr" = regs // [(c,regs!a * regs!b)]
  | op == "muli" = regs // [(c,regs!a * b)]
  | op == "banr" = regs // [(c,regs!a .&. regs!b)]
  | op == "bani" = regs // [(c,regs!a .&. b)]
  | op == "borr" = regs // [(c,regs!a .|. regs!b)]
  | op == "bori" = regs // [(c,regs!a .|. b)]
  | op == "setr" = regs // [(c,regs!a)]
  | op == "seti" = regs // [(c,a)]
  | op == "gtir" = regs // [(c,if a > regs!b then 1 else 0)]
  | op == "gtri" = regs // [(c,if regs!a > b then 1 else 0)]
  | op == "gtrr" = regs // [(c,if regs!a > regs!b then 1 else 0)]
  | op == "eqir" = regs // [(c,if a == regs!b then 1 else 0)]
  | op == "eqri" = regs // [(c,if regs!a == b then 1 else 0)]
  | op == "eqrr" = regs // [(c,if regs!a == regs!b then 1 else 0)]

run :: Array Int Insn -> State -> State
run insns state = either id (run insns) (step insns state)

testData :: String
testData = "#ip 0\nseti 5 0 1\nseti 6 0 2\naddi 0 1 0\naddr 1 2 3\nsetr 1 0 0\nseti 8 0 4\nseti 9 0 5"

test :: ()
test
  | flip run initState (parse testData) /= (7,listArray (-1,5) [0,6,5,6,0,0,9]) = error "a"
  | otherwise = ()

part1 :: IO Int
part1 =
    fmap ((!0) . snd . flip run initState . parse) $ readFile "input/19.txt"

initState2 :: State
initState2 = (0,listArray (-1,5) [0,1,0,0,0,0,0])

-- My input has the r4 bound to ip and the initialization code setting
-- r0=0 r1=0 r2=989 r3=0 r5=62 when r0=0
-- r0=0 r1=0 r2=10551298 r3=0 r5=10550400

-- Decompiling my input, it looks like r0 is the sum of all numbers that
-- divide r2.

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

factors :: Int -> [Int]
factors n = f primes n []
  where
    f [] _ _ = []
    f (p:ps) n facts
      | p > n = facts
      | n `mod` p /= 0 = f ps n facts
      | otherwise = f (p:ps) (n `div` p) (p:facts)

divisors :: Int -> [Int]
divisors n = d (group $ factors n)
  where
    d [] = [1]
    d (ns@(n:_):rest) = [ n^p*f | p <- [0..length ns], f <- d rest]

part2 :: [Int]
part2 = map (sum . divisors) [898,10551298]
