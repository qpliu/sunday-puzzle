module AOC201623 where

import Data.Array(Array,array,bounds,inRange,(!),(//))

import AOC

aoc = AOC {
    day="../../2016/input/23",
    aocTests=[
        AOCTest {
            testData=unlines [
                "cpy 2 a",
                "tgl a",
                "tgl a",
                "tgl a",
                "cpy 1 a",
                "dec a",
                "dec a"
                ],
            testResult=Just "3",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result 7,
        codeTest2=result2 12,
        codeResult=result 7,
        codeResult2=result2 12
        }
    }

data Insn = Insn (State -> State) Insn
type Operand = Either Int Char
type Registers = Array Char Int
type State = (Int,Registers,Array Int Insn)

cpy :: Operand -> Operand -> Insn
cpy arg1 arg2@(Left _) =
    Insn (\ (ip,regs,insns) -> (ip+1,regs,insns))
        $ jnz arg1 arg2
cpy arg1@(Left v1) arg2@(Right r2) =
    Insn (\ (ip,regs,insns) -> (ip+1,regs // [(r2,v1)],insns))
        $ jnz arg1 arg2
cpy arg1@(Right r1) arg2@(Right r2) =
    Insn (\ (ip,regs,insns) -> (ip+1,regs // [(r2,regs!r1)],insns))
        $ jnz arg1 arg2

inc :: Operand -> Insn
inc arg@(Left _) =
    Insn (\ (ip,regs,insns) -> (ip+1,regs,insns))
        $ dec arg
inc arg@(Right r) =
    Insn (\ (ip,regs,insns) -> (ip+1,regs // [(r,1+regs!r)],insns))
        $ dec arg

dec :: Operand -> Insn
dec arg@(Left _) =
    Insn (\ (ip,regs,insns) -> (ip+1,regs,insns))
        $ inc arg
dec arg@(Right r) =
    Insn (\ (ip,regs,insns) -> (ip+1,regs // [(r,regs!r-1)],insns))
        $ inc arg

jnz :: Operand -> Operand -> Insn
jnz arg1@(Left 0) arg2 =
    Insn (\ (ip,regs,insns) -> (ip+1,regs,insns))
        $ cpy arg1 arg2
jnz arg1@(Left _) arg2@(Left v2) =
    Insn (\ (ip,regs,insns) -> (ip+v2,regs,insns))
        $ cpy arg1 arg2
jnz arg1@(Left _) arg2@(Right r2) =
    Insn (\ (ip,regs,insns) -> (ip+regs!r2,regs,insns))
        $ cpy arg1 arg2
jnz arg1@(Right r1) arg2@(Left v2) =
    Insn (\ (ip,regs,insns) ->
            (ip+(if regs!r1 == 0 then 1 else v2),regs,insns))
        $ cpy arg1 arg2
jnz arg1@(Right r1) arg2@(Right r2) =
    Insn (\ (ip,regs,insns) ->
            (ip+(if regs!r1 == 0 then 1 else regs!r2),regs,insns))
        $ cpy arg1 arg2

tgl :: Operand -> Insn
tgl arg@(Left v) = 
    Insn (\ (ip,regs,insns) -> (ip+1,regs,toggle insns (ip+v)))
        $ inc arg
tgl arg@(Right r) =
    Insn (\ (ip,regs,insns) -> (ip+1,regs,toggle insns (ip+regs!r)))
        $ inc arg

toggle :: Array Int Insn -> Int -> Array Int Insn
toggle insns i
  | not (inRange (bounds insns) i) = insns
  | otherwise = insns // [(i,toggled)]
  where Insn _ toggled = insns!i

parseInsn :: [String] -> Insn
parseInsn ["cpy",a,b] = cpy (operand a) (operand b)
parseInsn ["inc",a] = inc (operand a)
parseInsn ["dec",a] = dec (operand a)
parseInsn ["jnz",a,b] = jnz (operand a) (operand b)
parseInsn ["tgl",a] = tgl (operand a)

operand :: String -> Either Int Char
operand arg
  | null num = Right $ head arg
  | otherwise = Left $ head num
  where num = parseInts arg

parse input = array (0,length insns-1) $ zip [0..] insns
  where insns = map (parseInsn . words) $ lines input

exec :: Int -> State -> Registers
exec tick state@(ip,regs,insns)
  | not (inRange (bounds insns) ip) = regs
  | otherwise = exec (tick+1) (insn state)
  where Insn insn _ = insns!ip

result a insns =
    (!'a') $ exec 0 (0,array ('a','d') (zip "abcd" $ a:repeat 0),insns)

{-
Decompiling:
b = a
b = b - 1
while b > 1
  a = a * b
  when b = 1,2,3,4, toggle changes one instruction after the while loop
end while
after the toggles:
a = a + parameter19*parameter20

a = factorial(a)+param19*param20
-}

parse2 = parseInts . concatMap snd
                   . filter ((`elem` [19,20]) . fst) . zip [0..] . lines

result2 a = (product [1..a]+) . product
