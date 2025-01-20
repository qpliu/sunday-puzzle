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

cup :: Int -> Vec s -> Int -> ST s Int
cup maxCup cups i = Vector.read cups (i `mod` maxCup)

play :: (Vec s -> Vec s -> ST s Int) -> Int -> Int -> [Int] -> ST s Int
play getResult maxCup nMoves starters = do
    cups <- Vector.new maxCup
    sequence_ [write cups i (i+1) | i <- [0..maxCup-1]]
    sequence_ [write cups i cup | (i,cup) <- zip [0..] starters]
    pos <- Vector.new (maxCup+1)
    sequence_ [write pos i (i-1) | i <- [1..maxCup]]
    sequence_ [write pos cup i | (i,cup) <- zip [0..] starters]
    moves maxCup nMoves cups pos
    getResult cups pos

moves :: Int -> Int -> Vec s -> Vec s -> ST s ()
moves maxCup nMoves cups pos = m 0 0
  where
    m nMove currentPos
      | nMove >= nMoves = return ()
      | otherwise = do
          current <- cup maxCup cups currentPos
          c1 <- cup maxCup cups (currentPos+1)
          c2 <- cup maxCup cups (currentPos+2)
          c3 <- cup maxCup cups (currentPos+3)
          let dest = findDest c1 c2 c3 ((current-2) `mod` maxCup + 1)
          destPos <- Vector.read pos dest

          if destPos > currentPos
            then do
              mapM_ back3 [currentPos+1..destPos-4]
              move dest (destPos-3)
              move c1 (destPos-2)
              move c2 (destPos-1)
              move c3 destPos
              m (nMove+1) ((currentPos+1) `mod` maxCup)
            else do
              mapM_ forward3 [currentPos+2,currentPos+1..destPos+4]
              move c1 (destPos+1)
              move c2 (destPos+2)
              move c3 (destPos+3)
              move current (currentPos+3)
              m (nMove+1) ((currentPos+4) `mod` maxCup)

    findDest c1 c2 c3 c
      | c /= c1 && c /= c2 && c /= c3 = c
      | otherwise = findDest c1 c2 c3 ((c-2) `mod` maxCup + 1)

    move c i = do
        write cups (i `mod` maxCup) c
        write pos c (i `mod` maxCup)

    back3 i = do
        c <- cup maxCup cups ((i+3) `mod` maxCup)
        move c i

    forward3 i = do
        c <- cup maxCup cups ((i-3) `mod` maxCup)
        move c i

labels :: Vec s -> Vec s -> ST s Int
labels cups pos = do
    one <- Vector.read pos 1
    ns <- sequence [cup 9 cups (one+i) | i <- [1..8]]
    return $ sum $ zipWith (*) (reverse ns) [10^i | i <- [0..]]

result starters = runST $ play labels 9 100 starters

labels2 :: Vec s -> Vec s -> ST s Int
labels2 cups pos = do
    one <- Vector.read pos 1
    ns <- sequence [cup 1000000 cups (one+i) | i <- [1,2]]
    return $ product ns

result2 starters = runST $ play labels2 1000000 1000000 starters
