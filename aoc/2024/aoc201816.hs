module AOC201816 where

import Data.Array(Array,array,(!),(//))
import Data.Bits((.&.),(.|.))
import Data.List(partition)
import Data.Map(Map,adjust,fromList,toList)
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2018/input/16",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Before: [3, 2, 1, 1]",
                "9 2 1 2",
                "After:  [3, 2, 2, 1]"
                ],
            testResult=Just "1",
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

parse = map (map parseInts) . parseBlankLineSeparated

type Registers = Array Int Int

type Op = Int -> Int -> Int -> Registers -> Registers
ops :: [(String,Op)]
ops = [
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

check :: [[Int]] -> (String,Op) -> Bool
check [si,[_,a,b,c],so] (_,op) = o == op a b c i
  where
    i = array (0,3) (zip [0..3] si)
    o = array (0,3) (zip [0..3] so)

result = length . filter has3 . takeWhile (not . null)
  where has3 sample = 3 <= length (filter (check sample) ops)

sieve :: [[Int]] -> Map Int [(String,Op)] -> Map Int [(String,Op)]
sieve sample@[_,(num:_),_] = adjust (filter (check sample)) num

match :: [(Int,Op)] -> [(Int,[(String,Op)])] -> [(Int,[(String,Op)])]
      -> Array Int Op
match done [] [] = array (0,15) done
match done [] opcodes = match done matched unmatched
  where (matched,unmatched) = partition ((== 1) . length .  snd) opcodes
match done ((opcode,[(name,op)]):matched) opcodes =
    match ((opcode,op):done) matched
          (map (fmap (filter ((/= name) . fst))) opcodes)

interp :: Array Int Op -> Registers -> [[Int]] -> Registers
interp table r [] = r
interp table r ([opcode,a,b,c]:rest) =
    interp table ((table!opcode) a b c r) rest

result2 input = (interp table registers (last input))!0
  where
    registers = array (0,3) (zip [0..3] (repeat 0))
    samples = takeWhile (not . null) input
    opcodes = foldr sieve (fromList (zip [0..15] (repeat ops))) samples

    table = match [] [] $ toList opcodes
