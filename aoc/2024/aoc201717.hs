module AOC201717 where

import Control.Monad.ST(ST,runST)
import Control.Monad.Primitive(PrimState)
import Data.Vector.Unboxed.Mutable(MVector,new,write)
import qualified Data.Vector.Unboxed.Mutable as VM

import AOC

aoc = AOC {
    day="../../2017/input/17",
    aocTests=[
        AOCTest {
            testData="3",
            testResult=Just "638",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 2017 2017,
        codeTest2=undefined,
        codeResult=result 2017 2017,
        codeResult2=result2 50000000
        }
    }

parse = head . parseInts

spin :: MVector (PrimState (ST s)) Int -> Int -> Int -> ST s ()
spin nexts step i = do
    currentPos <- next (i-1) step
    nextPos <- VM.read nexts currentPos
    write nexts currentPos i
    write nexts i nextPos
  where
    next j nsteps
      | nsteps <= 0 = return j
      | otherwise = do
          nextj <- VM.read nexts j
          next nextj (nsteps-1)

result ninserts index step = runST $ do
    nexts <- new (ninserts+1)
    mapM_ (spin nexts step) [1..ninserts]
    VM.read nexts index

spin2 :: ((Int,Int),Int) -> Int -> Int -> Int
spin2 ((currentPos,currentSize),after0) ninserts step
  | currentSize > ninserts = after0
  | nextPos == 1 = spin2 ((nextPos,currentSize+1),currentSize) ninserts step
  | otherwise = spin2 ((nextPos,currentSize+1),after0) ninserts step
  where nextPos = (currentPos+step) `mod` currentSize + 1

result2 ninserts step = spin2 ((0,1),0) ninserts step
