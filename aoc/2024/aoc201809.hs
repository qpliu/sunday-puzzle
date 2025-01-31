module AOC201809 where

import Control.Monad.Primitive(PrimState)
import Control.Monad.ST(ST,runST)
import Data.Vector.Unboxed.Mutable(MVector,modify,new,write)
import qualified Data.Vector.Unboxed.Mutable as Vector

import AOC

aoc = AOC {
    day="../../2018/input/09",
    aocTests=[
        AOCTest {
            testData="10 players; last marble is worth 1618 points",
            testResult=Just "8317",
            testResult2=Nothing
            },
        AOCTest {
            testData="13 players; last marble is worth 7999 points",
            testResult=Just "146373",
            testResult2=Nothing
            },
        AOCTest {
            testData="17 players; last marble is worth 1104 points",
            testResult=Just "2764",
            testResult2=Nothing
            },
        AOCTest {
            testData="21 players; last marble is worth 6111 points",
            testResult=Just "54718",
            testResult2=Nothing
            },
        AOCTest {
            testData="30 players; last marble is worth 5807 points",
            testResult=Just "37305",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=parseInts,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

type Vec s = MVector (PrimState (ST s)) Int

start :: [Int] -> Int
start [nplayers,lastMarble] = runST $ do
    scores <- new nplayers
    next <- new (lastMarble+1)
    prev <- new (lastMarble+1)
    play nplayers scores lastMarble next prev

play :: Int -> Vec s -> Int -> Vec s -> Vec s -> ST s Int
play nplayers scores lastMarble next prev = turn 0 1
  where
    turn current marble
      | marble > lastMarble = do
          s <- mapM (Vector.read scores) [0..nplayers-1]
          return $ maximum s
      | marble `mod` 23 == 0 = do
          modify scores (+marble) (mod marble nplayers)
          m1 <- Vector.read prev current
          m2 <- Vector.read prev m1
          m3 <- Vector.read prev m2
          m4 <- Vector.read prev m3
          m5 <- Vector.read prev m4
          m6 <- Vector.read prev m5
          m7 <- Vector.read prev m6
          m8 <- Vector.read prev m7
          modify scores (+m7) (mod marble nplayers)
          write prev m6 m8
          write next m8 m6
          turn m6 (marble+1)
      | otherwise = do
          p1 <- Vector.read next current
          p2 <- Vector.read next p1
          write next marble p2
          write prev marble p1
          write next p1 marble
          write prev p2 marble
          turn marble (marble+1)

result = start

result2 [nplayers,lastMarble] = start [nplayers,lastMarble*100]
