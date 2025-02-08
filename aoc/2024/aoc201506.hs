module AOC201506 where

import Data.Array(Array,array,inRange,range,(//))
import Data.Bits(complement,popCount,xor,(.|.),(.&.))

import Control.Monad((>=>))
import Control.Monad.Primitive(PrimState)
import Control.Monad.ST(ST,runST)
import Data.Vector.Unboxed.Mutable(MVector,modify,new)
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as V

import AOC

aoc = AOC {
    day="../../2015/input/06",
    aocTests=[
        AOCTest {
            testData=unlines [
                "turn on 0,0 through 999,999",
                "toggle 0,0 through 999,0",
                "turn off 499,499 through 500,500"
                ],
            testResult=Just $ show (1000000-1000-4),
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "turn off 0,0 through 0,0",
                "turn on 0,0 through 0,0",
                "toggle 0,0 through 999,999"
                ],
            testResult=Nothing,
            testResult2=Just "2000001"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse2,
        pcodeTest=result,
        pcodeTest2=const result2,
        pcodeResult=result,
        pcodeResult2=const result2
        }
    }

parseInsn :: String -> (Int,Integer) -> (Int,Integer)
parseInsn str (y,row)
  | w1 == "toggle" = if inRange (y1,y2) y then (y,xor b row) else (y,row)
  | w2 == "on" = if inRange (y1,y2) y then (y,b .|. row) else (y,row)
  | otherwise = if inRange (y1,y2) y then (y,complement b .&. row) else (y,row)
  where
    (w1:w2:_) = words str
    [x1,y1,x2,y2] = parseInts str
    b = xor (2^(x2+1)-1) (2^x1-1)

parse = foldr (.) id . reverse . map parseInsn . lines

result ncpu f =
    parallelMapReduce ncpu (popCount . snd . f) sum [(i,0) | i <- [0..999]]

parseInsn2 :: String -> MVector (PrimState (ST s)) Int -> ST s ()
parseInsn2 str v
  | w1 == "toggle" = update (+2)
  | w2 == "on" = update (+1)
  | otherwise = update (max 0 . pred)
  where
    (w1:w2:_) = words str
    [x1,y1,x2,y2] = parseInts str
    update f =
        sequence_ [modify v f (x+y*1000) | x <- [x1..x2], y <- [y1..y2]]

parse2 = lines

result2 input = runST $ do
    v <- new (1000*1000)
    mapM_ ($ v) $ map parseInsn2 input
    vu <- V.unsafeFreeze v
    return $ V.foldr (+) 0 vu
