{-# LANGUAGE BangPatterns #-}
module AOC201705 where

import Control.Monad.Primitive(PrimState)
import Control.Monad.ST(ST,runST)
import Data.Vector.Unboxed.Mutable(MVector,write)
import qualified Data.Vector.Unboxed.Mutable as Vector
import Data.Vector.Unboxed(Vector,fromList,thaw)

import AOC

aoc = AOC {
    day="../../2017/input/05",
    aocTests=[
        AOCTest {
            testData=unlines [
                "0",
                "3",
                "0",
                "1",
                "-3"
                ],
            testResult=Just "5",
            testResult2=Just "10"
            }
        ],
    aocCode=Code {
        codeParse=fromList . parseInts,
        codeParse2=fromList . parseInts,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

countJmps :: Int -> Int -> MVector (PrimState (ST s)) Int -> ST s Int
countJmps !count !ip jmps
  | ip < 0 || ip >= Vector.length jmps = return count
  | otherwise = do
      offset <- Vector.read jmps ip
      write jmps ip (offset+1)
      countJmps (count+1) (ip+offset) jmps

result jumps = runST $ do
    jmps <- thaw jumps
    countJmps 0 0 jmps

countJmps2 :: MVector (PrimState (ST s)) Int -> ST s Int
countJmps2 jmps = count 0 0
  where
    l = Vector.length jmps
    count !n !ip
      | ip < 0 || ip >= l = return n
      | otherwise = do
          offset <- Vector.read jmps ip
          if offset >= 3
            then write jmps ip (offset-1)
            else write jmps ip (offset+1)
          count (n+1) (ip+offset)

result2 jumps = runST $ do
    jmps <- thaw jumps
    countJmps2 jmps
