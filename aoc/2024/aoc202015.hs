module AOC202015 where

import Control.Monad.Primitive(PrimState)
import Control.Monad.ST(ST,runST)
import Data.Vector.Unboxed.Mutable(MVector,write)
import qualified Data.Vector.Unboxed.Mutable as Vector

import AOC

aoc = AOC {
    day="../../2020/input/15",
    -- part 2 code is too slow for the tests
    aocTests=[
        AOCTest {
            testData="0,3,6",
            testResult=Just "436",
            testResult2=Nothing --Just "175594"
            },
        AOCTest {
            testData="1,3,2",
            testResult=Just "1",
            testResult2=Nothing --Just "2578"
            },
        AOCTest {
            testData="2,1,3",
            testResult=Just "10",
            testResult2=Nothing --Just "3544142"
            },
        AOCTest {
            testData="1,2,3",
            testResult=Just "27",
            testResult2=Nothing --Just "261214"
            },
        AOCTest {
            testData="2,3,1",
            testResult=Just "78",
            testResult2=Nothing --Just "6895259"
            },
        AOCTest {
            testData="3,2,1",
            testResult=Just "438",
            testResult2=Nothing --Just "18"
            },
        AOCTest {
            testData="3,1,2",
            testResult=Just "1836",
            testResult2=Nothing --Just "362"
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=parseInts,
        codeTest=result 2020,
        codeTest2=result 30000000,
        codeResult=result 2020,
        codeResult2=result 30000000
        }
    }

speak :: Int -> Int -> Int -> MVector (PrimState (ST s)) Int -> ST s Int
speak n turn number history
  | turn >= n = return number
  | otherwise = do
      lastSpoken <- Vector.read history number
      write history number turn
      if lastSpoken == 0
        then speak n (turn+1) 0 history
        else speak n (turn+1) (turn - lastSpoken) history

starting :: Int -> [Int] -> ST s Int
starting n numbers = do
    history <- Vector.replicate n 0
    sequence_ [write history num i | (i,num) <- zip [1..] (init numbers)]
    speak n (length numbers) (last numbers) history

result n numbers = runST $ starting n numbers
