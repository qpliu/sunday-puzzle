module AOC201814 where

import Control.Monad.Primitive(PrimState)
import Control.Monad.ST(ST,runST)
import Data.Vector.Unboxed.Mutable(MVector,new,write)
import qualified Data.Vector.Unboxed.Mutable as Vector

import Data.Array(Array,array,assocs,(!))
import Data.Char(chr,isDigit,ord)

import AOC

aoc = AOC {
    day="../../2018/input/14",
    aocTests=[
        AOCTest {
            testData="9",
            testResult=Just $ show "5158916779",
            testResult2=Nothing
            },
        AOCTest {
            testData="5",
            testResult=Just $ show "0124515891",
            testResult2=Nothing
            },
        AOCTest {
            testData="18",
            testResult=Just $ show "9251071085",
            testResult2=Nothing
            },
        AOCTest {
            testData="2018",
            testResult=Just $ show "5941429882",
            testResult2=Nothing
            },
        AOCTest {
            testData="51589",
            testResult=Nothing,
            testResult2=Just "9"
            },
        AOCTest {
            testData="01245",
            testResult=Nothing,
            testResult2=Just "5"
            },
        AOCTest {
            testData="92510",
            testResult=Nothing,
            testResult2=Just "18"
            },
        AOCTest {
            testData="59414",
            testResult=Nothing,
            testResult2=Just "2018"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse = head . parseInts

type Vec s = MVector (PrimState (ST s)) Int

score :: Int -> Vec s -> Int -> Int -> Int -> ST s ()
score nScores scores e1 e2 i
  | i >= nScores = return ()
  | otherwise = do
      s1 <- Vector.read scores e1
      s2 <- Vector.read scores e2
      if s1+s2 < 10
        then do
          write scores i (s1+s2)
          score nScores scores
                ((e1 + 1 + s1) `mod` (i+1))
                ((e2 + 1 + s2) `mod` (i+1))
                (i+1)
        else do
          write scores i ((s1+s2) `div` 10)
          if i+1 >= nScores
            then return ()
            else do
              write scores (i+1) ((s1+s2) `mod` 10)
              score nScores scores
                  ((e1 + 1 + s1) `mod` (i+2))
                  ((e2 + 1 + s2) `mod` (i+2))
                  (i+2)

result n = runST $ do
    scores <- new (n+10)
    write scores 0 3
    write scores 1 7
    score (n+10) scores 0 1 2
    lastScores <- mapM (Vector.read scores) [n..n+9]
    return $ map (chr . (+ (ord '0'))) lastScores

parse2 = reverse . map ((+ (- (ord '0'))) . ord) . filter isDigit

search :: Int -> [Int] -> Int -> [Int] -> Array Int (Vec s)
       -> Int -> Int -> Int
       -> ST s Int
search blockSize target targetLen latest blocks e1 e2 i = do
    s1 <- readScore e1
    s2 <- readScore e2
    if s1+s2 < 10
      then do
        let nextLatest = take targetLen ((s1+s2):latest)
        if nextLatest == target
          then return $ i+1-targetLen
          else do
            nextBlocks <- appendScore blocks i (s1+s2)
            search blockSize target targetLen nextLatest nextBlocks
                   ((e1 + 1 + s1) `mod` (i+1))
                   ((e2 + 1 + s2) `mod` (i+1))
                   (i+1)
      else do
        let nextLatest = take targetLen ((s1+s2) `div` 10:latest)
        if nextLatest == target
          then return $ i+1-targetLen
          else do
            nextBlocks <- appendScore blocks i ((s1+s2) `div` 10)
            let nextLatest2 = take targetLen ((s1+s2) `mod` 10:nextLatest)
            if nextLatest2 == target
              then return $ i+2-targetLen
              else do
                nextBlocks2 <- appendScore nextBlocks (i+1) ((s1+s2) `mod` 10)
                search blockSize target targetLen nextLatest2 nextBlocks2
                       ((e1 + 1 + s1) `mod` (i+2))
                       ((e2 + 1 + s2) `mod` (i+2))
                       (i+2)
  where
    readScore ie =
        Vector.read (blocks!(ie `div` blockSize)) (ie `mod` blockSize)
    appendScore blocks ie s
      | ie `mod` blockSize /= 0 = do
          write (blocks!(ie `div` blockSize)) (ie `mod` blockSize) s
          return blocks
      | otherwise = do
          block <- new blockSize
          write block 0 s
          return $ array (0,ie `div` blockSize)
                 $ (ie `div` blockSize,block):assocs blocks

result2 target = runST $ do
    block <- new blockSize
    write block 0 3
    write block 1 7
    search blockSize target (length target) [7,3] (array (0,0) [(0,block)])
           0 1 2
  where blockSize = 1000000
