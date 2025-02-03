module AOC201725 where

import Data.Map(Map,fromList,(!))

import AOC

aoc = AOC {
    day="../../2017/input/25",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Begin in state A.",
                "Perform a diagnostic checksum after 6 steps.",
                "",
                "In state A:",
                "  If the current value is 0:",
                "    - Write the value 1.",
                "    - Move one slot to the right.",
                "    - Continue with state B.",
                "  If the current value is 1:",
                "    - Write the value 0.",
                "    - Move one slot to the left.",
                "    - Continue with state B.",
                "",
                "In state B:",
                "  If the current value is 0:",
                "    - Write the value 1.",
                "    - Move one slot to the left.",
                "    - Continue with state A.",
                "  If the current value is 1:",
                "    - Write the value 1.",
                "    - Move one slot to the right.",
                "    - Continue with state A."
                ],
            testResult=Just "3",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=undefined,
        codeResult=result,
        codeResult2=const ()
        }
    }

parse input = (head (parseInts nsteps),table!init begin)
  where
    "Begin":"in":"state":begin:"Perform":"a":"diagnostic":"checksum":"after"
           :nsteps:"steps.":states = words input
    table = fromList $ parseStates states
    parseStates [] = []
    parseStates ("In":"state":state:
                 "If":"the":"current":"value":"is":"0:":
                 "-":"Write":"the":"value":value0:
                 "-":"Move":"one":"slot":"to":"the":move0:
                 "-":"Continue":"with":"state":state0:
                 "If":"the":"current":"value":"is":"1:":
                 "-":"Write":"the":"value":value1:
                 "-":"Move":"one":"slot":"to":"the":move1:
                 "-":"Continue":"with":"state":state1:rest) =
        (init state,makeInsn v0 move0 (table!init state0)
                             v1 move1 (table!init state1))
            : parseStates rest
      where
        [v0] = parseInts value0
        [v1] = parseInts value1

type State = ([Int],[Int])

makeInsn :: Int -> String -> (Int -> State -> Int)
         -> Int -> String -> (Int -> State -> Int)
         -> Int -> State -> Int
makeInsn v0 move0    insn0 v1 move1    insn1 0 (l,r) =
    sum l + sum r
makeInsn v0 "left."  insn0 v1 move1    insn1 t ([],[]) =
    insn0 (t-1) ([],[0,v0])
makeInsn v0 "left."  insn0 v1 move1    insn1 t (l0:l,[]) =
    insn0 (t-1) (l,[l0,v0])
makeInsn v0 "left."  insn0 v1 move1    insn1 t ([],0:r) =
    insn0 (t-1) ([],0:v0:r)
makeInsn v0 "left."  insn0 v1 move1    insn1 t (l0:l,0:r) =
    insn0 (t-1) (l,l0:v0:r)
makeInsn v0 "right." insn0 v1 move1    insn1 t (l,[]) =
    insn0 (t-1) (v0:l,[])
makeInsn v0 "right." insn0 v1 move1    insn1 t (l,0:r) =
    insn0 (t-1) (v0:l,r)
makeInsn v0 move0    insn0 v1 "left."  insn1 t ([],1:r) =
    insn1 (t-1) ([],0:v1:r)
makeInsn v0 move0    insn0 v1 "left."  insn1 t (l0:l,1:r) =
    insn1 (t-1) (l,l0:v1:r)
makeInsn v0 move0    insn0 v1 "right." insn1 t (l,1:r) =
    insn1 (t-1) (v1:l,r)
makeInsn v0 move0    insn0 v1 move1    insn1 t state = error $ show (v0,move0,v1,move1,t,state)

result (nsteps,exec) = exec nsteps ([],[])
