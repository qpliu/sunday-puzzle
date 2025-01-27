module AOC202023 where

import Control.Monad.Primitive(PrimState)
import Control.Monad.ST(ST,runST)
import Data.Vector.Unboxed.Mutable(MVector,write)
import qualified Data.Vector.Unboxed.Mutable as Vector

import Data.Char(isDigit,ord)

import AOC

aoc = AOC {
    day="../../2020/input/23",
    aocTests=[
        AOCTest {
            testData="389125467",
            testResult=Just "67384529",
            testResult2=Just "149245887792"
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

parse = map (abs . (ord '0' -) . ord) . filter isDigit

type Vec s = MVector (PrimState (ST s)) Int

play :: (Vec s -> ST s a) -> Int -> Int -> [Int] -> ST s a
play getResult maxCup nMoves starters = do
    next <- Vector.new (maxCup+1)
    sequence_ [write next i (i+1) | i <- [1..maxCup]]
    let afterStarters
          | maxCup == length starters = head starters
          | otherwise = length starters + 1
    sequence_ [write next i cup
               | (i,cup) <- zip starters (tail starters ++ [afterStarters])]
    if maxCup > length starters
      then write next maxCup (head starters)
      else return ()
    moves maxCup nMoves next (head starters)
    getResult next

moves :: Int -> Int -> Vec s -> Int -> ST s ()
moves maxCup nMoves next initialCurrent = m 0 initialCurrent
  where
    m nMove current
      | nMove >= nMoves = return ()
      | otherwise = do
          c1 <- Vector.read next current
          c2 <- Vector.read next c1
          c3 <- Vector.read next c2
          c4 <- Vector.read next c3
          let dest = findDest c1 c2 c3 ((current-2) `mod` maxCup + 1)
          d1 <- Vector.read next dest

          write next current c4
          write next dest c1
          write next c3 d1
          m (nMove+1) c4

    findDest c1 c2 c3 d
      | d /= c1 && d /= c2 && d /= c3 = d
      | otherwise = findDest c1 c2 c3 ((d-2) `mod` maxCup + 1)

labels :: Vec s -> ST s Int
labels next = do
    c2 <- Vector.read next 1
    c3 <- Vector.read next c2
    c4 <- Vector.read next c3
    c5 <- Vector.read next c4
    c6 <- Vector.read next c5
    c7 <- Vector.read next c6
    c8 <- Vector.read next c7
    c9 <- Vector.read next c8
    return $ sum $ zipWith (*) [c9,c8,c7,c6,c5,c4,c3,c2] [10^i | i <- [0..]]

result starters = runST $ play labels 9 100 starters

labels2 :: Vec s -> ST s Int
labels2 next = do
    c2 <- Vector.read next 1
    c3 <- Vector.read next c2
    return $ c2*c3

result2 starters = runST $ play labels2 1000000 10000000 starters
