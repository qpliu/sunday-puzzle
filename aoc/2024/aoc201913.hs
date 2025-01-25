module AOC201913 where

import Data.Map(Map,elems,empty,insert)
import Data.Vector.Unboxed((//))

import AOC
import AOC2019

aoc = AOC {
    day="../../2019/input/13",
    aocTests=[],
    aocCode=Code {
        codeParse=parseIntCode,
        codeParse2=parseIntCode,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

type XY = (Int,Int)
type Out = (Int,Int)
type State = (Out,(Map XY Int,Int,Int,Int,Int))

initIO :: IntCodeIO Out
initIO = IntCodeIO (outX ((0,1),(empty,0,0,0,0)))

outX :: State
     -> Maybe Int -> [Int] -> Maybe (IntCodeIO Out -> [Int] -> Out) -> Out
outX state@(out,(screen,paddle,ball,x,y)) output input = maybe out continue
  where
    Just newX = output
    continue cont
      | output == Nothing = cont (IntCodeIO (outX state)) input
      | otherwise = cont (IntCodeIO (outY newState)) []
    newState = (out,(screen,paddle,ball,newX,y))

outY :: State
     -> Maybe Int -> [Int] -> Maybe (IntCodeIO Out -> [Int] -> Out) -> Out
outY state@(out,(screen,paddle,ball,x,y)) output input = maybe out continue
  where
    Just newY = output
    continue cont
      | output == Nothing = cont (IntCodeIO (outY state)) input
      | otherwise = cont (IntCodeIO (outT newState)) []
    newState = (out,(screen,paddle,ball,x,newY))

outT :: State
     -> Maybe Int -> [Int] -> Maybe (IntCodeIO Out -> [Int] -> Out) -> Out
outT state@(out@(score,nblocks),(screen,paddle,ball,x,y)) output input
  | nblocks == 0 = maybe out (const newOut)
  | otherwise = maybe newOut continue
  where
    Just tile = output
    continue cont
      | output == Nothing = cont (IntCodeIO (outT state)) input
      | otherwise = cont (IntCodeIO (outX newState)) [joystick]
    newOut = (newScore,newNBlocks)
    newState = (newOut,(newScreen,newPaddle,newBall,0,0))

    newScore | x < 0 = tile | otherwise = score
    newNBlocks
      | tile == 2 = length $ filter (== 2) $ elems newScreen
      | otherwise = nblocks
    newScreen | x < 0 = screen | otherwise = insert (x,y) tile screen
    newPaddle | tile == 3 = x | otherwise = paddle
    newBall | tile == 4 = x | otherwise = ball
    joystick = signum (newBall - newPaddle)

result = snd . intCodeIO initIO []

result2 = fst . intCodeIO initIO [] . (// [(0,2)])
