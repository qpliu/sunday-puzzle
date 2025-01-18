module AOC202008 where

import Data.Map(Map,fromList,insert,member,singleton,(!))

import AOC

aoc = AOC {
    day="../../2020/input/08",
    aocTests=[
        AOCTest {
            testData=unlines [
                "nop +0",
                "acc +1",
                "jmp +4",
                "acc +3",
                "jmp -3",
                "acc -99",
                "acc +1",
                "jmp -4",
                "acc +6"
                ],
            testResult=Just "5",
            testResult2=Just "8"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

data State = State Int Int (Map Int (State -> Int))
type Insn = State -> Int

halt :: Insn
halt (State acc _ _) = acc

nop :: Insn
nop (State acc pc insns) =
    (insns!(pc+1)) (State acc (pc+1) (insert pc halt insns))

acc :: Int -> Insn
acc n (State acc pc insns) =
    (insns!(pc+1)) (State (acc+n) (pc+1) (insert pc halt insns))

jmp :: Int -> Insn
jmp n (State acc pc insns) =
    (insns!(pc+n)) (State acc (pc+n) (insert pc halt insns))

parse = fromList . zip [0..] . map (p . words) . lines
  where
    p ["nop",_] = nop
    p ["acc",'+':arg] = acc (read arg)
    p ["acc",arg] = acc (read arg)
    p ["jmp",'+':arg] = jmp (read arg)
    p ["jmp",arg] = jmp (read arg)

result insns = (insns!0) (State 0 0 insns)

type State2 = (Int,Int,Maybe (Map Int (),Int))

parse2 input = insns
  where
    insns = fromList $ zip [0..] $ (map (p . words) (lines input)) ++ [halt2]
    p ["nop",'+':arg] = nop2 (read arg)
    p ["nop",arg] = nop2 (read arg)
    p ["acc",'+':arg] = acc2 (read arg)
    p ["acc",arg] = acc2 (read arg)
    p ["jmp",'+':arg] = jmp2 (read arg)
    p ["jmp",arg] = jmp2 (read arg)

    halt2 :: State2 -> Int
    halt2 (acc,_,_) = acc

    nop2 :: Int -> State2 -> Int
    nop2 n (acc,pc,Nothing) =
        (insns!(pc+n)) (acc,pc+n,Just (singleton pc (),
                                       (insns!(pc+1)) (acc,pc+1,Nothing)))
    nop2 n (acc,pc,Just (seen,continuation))
      | member pc seen = continuation
      | otherwise =
          (insns!(pc+1)) (acc,pc+1,Just (insert pc () seen,continuation))

    acc2 :: Int -> State2 -> Int
    acc2 n (acc,pc,speculation) = (insns!(pc+1)) (acc+n,pc+1,speculation)

    jmp2 :: Int -> State2 -> Int
    jmp2 n (acc,pc,Nothing) =
        (insns!(pc+1)) (acc,pc+1,Just (singleton pc (),
                                       (insns!(pc+n)) (acc,pc+n,Nothing)))
    jmp2 n (acc,pc,Just (seen,continuation))
      | member pc seen = continuation
      | otherwise =
          (insns!(pc+n)) (acc,pc+n,Just (insert pc () seen,continuation))

result2 insns = (insns!0) (0,0,Nothing)
