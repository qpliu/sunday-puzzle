module AOC201819 where

import Data.Array(Array,array,(!),(//))
import Data.Bits((.&.),(.|.))
import Data.List(subsequences)
import Data.Map(Map,fromList)
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2018/input/19",
    aocTests=[
        AOCTest {
            testData=unlines [
                "#ip 0",
                "seti 5 0 1",
                "seti 6 0 2",
                "addi 0 1 0",
                "addr 1 2 3",
                "setr 1 0 0",
                "seti 8 0 4",
                "seti 9 0 5"
                ],
            testResult=Just "6",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=resultTest,
        codeTest2=undefined,
        codeResult=result 0,
        codeResult2=result 1
        }
    }

parse input = (ip,len,array (0,len-1) $ zip [0..] $ map p code)
  where
    (pragma:code) = lines input
    [ip] = parseInts pragma
    len = length code
    p insn
      | not (Data.Map.member (take 4 insn) ops) = error insn
      | otherwise = (ops Data.Map.! take 4 insn) a b c
      where [a,b,c] = parseInts insn

type Registers = Array Int Int
type Op = Int -> Int -> Int -> Registers -> Registers

ops :: Map String Op
ops = fromList [
    ("addr",\ a b c r -> r // [(c,(r!a+r!b))]),
    ("addi",\ a b c r -> r // [(c,(r!a+b))]),
    ("mulr",\ a b c r -> r // [(c,(r!a*r!b))]),
    ("muli",\ a b c r -> r // [(c,(r!a*b))]),
    ("banr",\ a b c r -> r // [(c,(r!a.&.r!b))]),
    ("bani",\ a b c r -> r // [(c,(r!a.&.b))]),
    ("borr",\ a b c r -> r // [(c,(r!a.|.r!b))]),
    ("bori",\ a b c r -> r // [(c,(r!a.|.b))]),
    ("setr",\ a b c r -> r // [(c,(r!a))]),
    ("seti",\ a b c r -> r // [(c,a)]),
    ("gtir",\ a b c r -> r // [(c,(bool (a>r!b)))]),
    ("gtri",\ a b c r -> r // [(c,(bool (r!a>b)))]),
    ("gtrr",\ a b c r -> r // [(c,(bool (r!a>r!b)))]),
    ("eqir",\ a b c r -> r // [(c,(bool (a==r!b)))]),
    ("eqri",\ a b c r -> r // [(c,(bool (r!a==b)))]),
    ("eqrr",\ a b c r -> r // [(c,(bool (r!a==r!b)))])
    ]
  where
    bool t | t = 1 | otherwise = 0

interp :: Bool -> Registers -> (Int,Int,Array Int (Registers -> Registers))
       -> Registers
interp earlyExit initialRegisters (ipRegister,ipMax,insns) =
    i 0 initialRegisters
  where
    i ip registers
      | earlyExit && ip == 1 = registers
      | ip < 0 || ip >= ipMax = registers
      | otherwise = i (newRegisters!ipRegister + 1) newRegisters
      where newRegisters = (insns!ip) (registers // [(ipRegister,ip)])

registers :: Int -> Registers
registers r0 = array (0,5) $ zip [0..5] (r0:repeat 0)

resultTest = (!0) . interp False (registers 0)

{- Decompiling my input
0 GOTO 17
1 R1 = 1
2 R3 = 1
3 IF R1*R3 = R2 GOTO 7
6 GOTO 8
7 R0 = R1 + R0
8 R3 = R3 + 1
9 IF R3 > R2 GOTO 12
11 GOTO 3
12 R1 = R1 + 1
13 IF R1 > R2 GOTO 16
15 GOTO 2
16 HALT

17 initialize R2 to some value

25 IF R0 = 1 GOTO 27
26 GOTO 1

27 set R2 to a much larger value
34 R0 = 0
35 GOTO 1

so R0 is the sum of all divisors of R2
-}

primes :: [Int]
primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

factors :: Int -> [Int]
factors n = f n primes
  where
    f 1 _ = []
    f n (p:ps)
      | n `mod` p == 0 = p : f (n `div` p) (p:ps)
      | otherwise = f n ps

result r0 = sum . map product . subsequences . factors
                . (!2) . interp True (registers r0)
