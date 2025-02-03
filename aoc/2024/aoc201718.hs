module AOC201718 where

import Data.Array(Array,array,(!))
import Data.Map(Map,empty,findWithDefault,insert,singleton)

import AOC

aoc = AOC {
    day="../../2017/input/18",
    aocTests=[
        AOCTest {
            testData=unlines [
                "set a 1",
                "add a 2",
                "mul a a",
                "mod a 5",
                "snd a",
                "set a 0",
                "rcv a",
                "jgz a -1",
                "set a 1",
                "jgz a -2"
                ],
            testResult=Just "4",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "snd 1",
                "snd 2",
                "snd p",
                "rcv a",
                "rcv b",
                "rcv c",
                "rcv d"
                ],
            testResult=Nothing,
            testResult2=Just "3"
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

type State = (Int,Map String Int)
type Insn = State -> Either (Int,Int -> State) (Maybe Int,State)

parseInsn (op:x:y)
  | op == "snd" && null xval =
      \ (ip,regs) -> Right (Just $ findWithDefault 0 x regs,(ip+1,regs))
  | op == "snd" =
      \ (ip,regs) -> Right (Just $ head xval,(ip+1,regs))
  | op == "set" && null yval =
      \ (ip,regs) ->
          let yval = findWithDefault 0 (head y) regs
          in  Right (Nothing,(ip+1,insert x yval regs))
  | op == "set" =
      \ (ip,regs) -> Right (Nothing,(ip+1,insert x (head yval) regs))
  | op == "add" && null yval =
      \ (ip,regs) ->
          let xval = findWithDefault 0 x regs
              yval = findWithDefault 0 (head y) regs
          in  Right (Nothing,(ip+1,insert x (xval+yval) regs))
  | op == "add" =
      \ (ip,regs) ->
          let xval = findWithDefault 0 x regs
          in  Right (Nothing,(ip+1,insert x (xval+head yval) regs))
  | op == "mul" && null yval =
      \ (ip,regs) ->
          let xval = findWithDefault 0 x regs
              yval = findWithDefault 0 (head y) regs
          in  Right (Nothing,(ip+1,insert x (xval*yval) regs))
  | op == "mul" =
      \ (ip,regs) ->
          let xval = findWithDefault 0 x regs
          in  Right (Nothing,(ip+1,insert x (xval*head yval) regs))
  | op == "mod" && null yval =
      \ (ip,regs) ->
          let xval = findWithDefault 0 x regs
              yval = findWithDefault 0 (head y) regs
          in  Right (Nothing,(ip+1,insert x (xval `mod` yval) regs))
  | op == "mod" =
      \ (ip,regs) ->
          let xval = findWithDefault 0 x regs
          in  Right (Nothing,(ip+1,insert x (xval `mod` head yval) regs))
  | op == "rcv" =
      \ (ip,regs) ->
          Left (findWithDefault 0 x regs,\ rcv -> (ip+1,insert x rcv regs))
  | op == "jgz" && null xval && null yval =
      \ (ip,regs) ->
          let xval = findWithDefault 0 x regs
              yval = findWithDefault 0 (head y) regs
          in  if xval > 0
                then Right (Nothing,(ip+yval,regs))
                else Right (Nothing,(ip+1,regs))
  | op == "jgz" && null xval =
      \ (ip,regs) ->
          let xval = findWithDefault 0 x regs
          in  if xval > 0
                then Right (Nothing,(ip+head yval,regs))
                else Right (Nothing,(ip+1,regs))
  | op == "jgz" && null yval =
          if head xval > 0
            then \ (ip,regs) ->
              let yval = findWithDefault 0 (head y) regs
              in  Right (Nothing,(ip+yval,regs))
            else \ (ip,regs) -> Right (Nothing,(ip+1,regs))
  | op == "jgz" =
          if head xval > 0
            then \ (ip,regs) -> Right (Nothing,(ip+head yval,regs))
            else \ (ip,regs) -> Right (Nothing,(ip+1,regs))
  where
    xval = parseInts x
    yval = parseInts $ head y

parse input = array (0,length insns-1) $ zip [0..] insns
  where
    insns = map (parseInsn . words) $ lines input

interp :: Array Int Insn -> State -> Int -> Int
interp insns state@(ip,_) sound = continue $ (insns!ip) state
  where
    continue (Right (Just nextSound,nextState)) =
        interp insns nextState nextSound
    continue (Right (Nothing,nextState)) =
        interp insns nextState sound
    continue (Left (x,makeNextState))
      | x == 0 || sound == 0 = interp insns (makeNextState 0) sound
      | otherwise = sound

result insns = interp insns (0,empty) 0

scheduler :: Array Int Insn
          -> ([Int],Either State (Int -> State))
          -> ([Int],Either State (Int -> State))
          -> Int -> Int -> Int
scheduler insns (in0:q0,Right makeState0) state1 count tick =
    continue0 insns q0 ((insns!ip0) state0) state1 count tick
  where state0@(ip0,_) = makeState0 in0
scheduler insns state0 (in1:q1,Right makeState1) count tick =
    continue1 insns state0 q1 ((insns!ip1) state1) count tick
  where state1@(ip1,_) = makeState1 in1
scheduler insns state0 (q1,Left state1@(ip1,_)) count tick =
    continue1 insns state0 q1 ((insns!ip1) state1) count tick
scheduler insns (q0,Left state0@(ip0,_)) state1 count tick =
    continue0 insns q0 ((insns!ip0) state0) state1 count tick
scheduler insns ([],_) ([],_) count tick = count

continue0 :: Array Int Insn
          -> [Int]
          -> Either (Int,Int -> State) (Maybe Int,State)
          -> ([Int],Either State (Int -> State))
          -> Int -> Int -> Int
continue0 insns q0 (Left (_,makeState0)) state1 count tick =
    scheduler insns (q0,Right makeState0) state1 count (tick+1)
continue0 insns q0 (Right (Nothing,state0)) state1 count tick =
    scheduler insns (q0,Left state0) state1 count (tick+1)
continue0 insns q0 (Right (Just out,state0)) (q1,state1) count tick =
    scheduler insns (q0,Left state0) (q1++[out],state1) count (tick+1)

continue1 :: Array Int Insn
          -> ([Int],Either State (Int -> State))
          -> [Int]
          -> Either (Int,Int -> State) (Maybe Int,State)
          -> Int -> Int -> Int
continue1 insns state0 q1 (Left (_,makeState1)) count tick =
    scheduler insns state0 (q1,Right makeState1) count (tick+1)
continue1 insns state0 q1 (Right (Nothing,state1)) count tick =
    scheduler insns state0 (q1,Left state1) count (tick+1)
continue1 insns (q0,state0) q1 (Right (Just out,state1)) count tick =
    scheduler insns (q0++[out],state0) (q1,Left state1) (count+1) (tick+1)

result2 insns =
    scheduler insns ([],Left (0,empty)) ([],Left (0,singleton "p" 1)) 0 0
