module AOC201923 where

import Data.Map(Map,adjust,empty,insert,(!))
import Data.Vector.Unboxed(Vector)

import AOC
import AOC2019

aoc = AOC {
    day="../../2019/input/23",
    aocTests=[],
    aocCode=Code {
        codeParse=parseIntCode,
        codeParse2=parseIntCode,
        codeTest=undefined,
        codeTest2=undefined,
        codeResult=result,
        codeResult2=result2
        }
    }

type NodeState = ([Int],Maybe Int,[Int],IntCodeIO Int -> [Int] -> Int)
type State = Map Int NodeState

initNetwork :: (State -> Int) -> Int -> State -> Vector Int -> Int
initNetwork startNetwork addr state mem
  | addr < 50 = intCodeIO (IntCodeIO continue) [addr] mem
  | otherwise = startNetwork state
  where
    continue output input (Just continuation) =
        initNetwork startNetwork (addr+1)
                    (insert addr ([],output,input,continuation) state) mem

scheduler :: (Int -> state -> (state -> Int) -> Int) -> Int -> state -> Int
scheduler task addr state = task addr state continueScheduler
  where
    continueScheduler contState = scheduler task ((addr+1) `mod` 50) contState

nodeIO :: Int -> State -> (State -> Int) -> Int
nodeIO addr state yield
  | out == Nothing && null inQ = cont (IntCodeIO (continue outQ state)) [-1]
  | out == Nothing = cont (IntCodeIO (continue outQ state)) inQ
  | length outQ < 2 = cont (IntCodeIO (continue (outNum:outQ) state)) inQ
  | destAddr == 255 = outNum
  | otherwise = cont (IntCodeIO (continue [] (adjust enQ destAddr state))) inQ
  where
    (outQ,out,inQ,cont) = state!addr
    continue newOutQ newState newOut newInQ (Just continuation) =
        yield $ insert addr (newOutQ,newOut,newInQ,continuation) newState

    Just outNum = out
    [outX,destAddr] = outQ
    enQ (destOutQ,destOut,destInQ,destCont) =
        (destOutQ,destOut,destInQ ++ [outX,outNum],destCont)

result = initNetwork (scheduler nodeIO 0) 0 empty

type State2 = (Map Int NodeState,(Int,Int),Maybe Int)

initNat :: State -> State2
initNat state = (state,(0,0),Nothing)

nat :: Int -> State2 -> (State2 -> Int) -> Int
nat addr state2@(state,xy255@(x255,y255),lastY) yield
  | addr == 0 && all idle state = resume
  | out == Nothing && null inQ =
      cont (IntCodeIO (continue outQ state xy255 lastY)) [-1]
  | out == Nothing =
      cont (IntCodeIO (continue outQ state xy255 lastY)) inQ
  | length outQ < 2 =
      cont (IntCodeIO (continue (outNum:outQ) state xy255 lastY)) inQ
  | destAddr == 255 =
      cont (IntCodeIO (continue [] state (outX,outNum) lastY)) inQ
  | otherwise =
      cont (IntCodeIO (continue [] (adjust enQ destAddr state) xy255 lastY)) inQ
  where
    idle (_,nodeOut,nodeInQ,_) = nodeOut == Nothing && null nodeInQ
    resume
      | lastY == Just y255 = y255
      | otherwise =
          cont (IntCodeIO (continue outQ state xy255 (Just y255))) [x255,y255]

    (outQ,out,inQ,cont) = state!addr
    continue newOutQ newState newXY255 newLastY newOut newInQ
             (Just continuation) =
        yield (insert addr (newOutQ,newOut,newInQ,continuation) newState,
               newXY255,newLastY)

    Just outNum = out
    [outX,destAddr] = outQ
    enQ (destOutQ,destOut,destInQ,destCont) =
        (destOutQ,destOut,destInQ ++ [outX,outNum],destCont)

result2 = initNetwork (scheduler nat 0 . initNat) 0 empty
