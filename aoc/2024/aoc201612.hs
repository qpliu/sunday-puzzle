module AOC201612 where

import Data.Array(Array,array,bounds,inRange,(!),(//))

import AOC

aoc = AOC {
    day="../../2016/input/12",
    aocTests=[
        AOCTest {
            testData=unlines [
                "cpy 41 a",
                "inc a",
                "inc a",
                "dec a",
                "jnz a 2",
                "dec a"
                ],
            testResult=Just "42",
            testResult2=Nothing
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

type State = (Int,Array Char Int)

parseInsn :: [String] -> State -> State
parseInsn (op:operands)
  | op == "cpy" && null val1 =
        \ (ip,regs) -> (ip+1,regs // [(reg2,regs!reg1)])
  | op == "cpy" =
        \ (ip,regs) -> (ip+1,regs // [(reg2,head val1)])
  | op == "inc" =
        \ (ip,regs) -> (ip+1,regs // [(reg1,regs!reg1 + 1)])
  | op == "dec" =
        \ (ip,regs) -> (ip+1,regs // [(reg1,regs!reg1 - 1)])
  | op == "jnz" && null val1 && null val2 =
        \ (ip,regs) ->
            if regs!reg1 == 0 then (ip+1,regs) else (ip+regs!reg2,regs)
  | op == "jnz" && null val1 =
        \ (ip,regs) ->
            if regs!reg1 == 0 then (ip+1,regs) else (ip+head val2,regs)
  | op == "jnz" && head val1 == 0 =
        \ (ip,regs) -> (ip+1,regs)
  | op == "jnz" && null val2 =
        \ (ip,regs) -> (ip+regs!reg2,regs)
  | op == "jnz" =
        \ (ip,regs) -> (ip+head val2,regs)
  where
    (operand1:_) = operands
    [reg1] = operand1
    val1 = parseInts operand1
    (_:operand2:_) = operands
    [reg2] = operand2
    val2 = parseInts operand2

parse input = array (0,length insns-1) $ zip [0..] insns
  where insns = map (parseInsn . words) $ lines input

exec :: State -> Array Int (State -> State) -> State
exec state@(ip,_) insns
  | not (inRange (bounds insns) ip) = state
  | otherwise = exec ((insns!ip) state) insns

result = (!'a') . snd . exec (0,array ('a','d') $ zip "abcd" [0,0,0,0])

result2 = (!'a') . snd . exec (0,array ('a','d') $ zip "abcd" [0,0,1,0])
