module AOC2019 where

import Control.Monad.ST(runST)
import Data.Vector.Unboxed(Vector,modify,(!),(!?),(//))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import AOC

parseIntCode :: String -> Vector Int
parseIntCode = V.fromList . parseInts

data IntCodeIO a =
    IntCodeIO (Maybe Int -> [Int] -> Maybe (IntCodeIO a -> [Int] -> a) -> a)

intCode0 :: Vector Int -> Int
intCode0 = intCodeLoop (IntCodeIO io) [] (0,0)
  where io (Just val) _ _ = val

intCode :: [Int] -> Vector Int -> [Int]
intCode input = intCodeLoop (IntCodeIO io) input (0,0)
  where io val input continuation = maybe [] continue continuation
          where continue f = maybe id (:) val $ f (IntCodeIO io) input

intCodeIO :: IntCodeIO a -> [Int] -> Vector Int -> a
intCodeIO io input mem = intCodeLoop io input (0,0) mem

intCodeLoop :: IntCodeIO a -> [Int] -> (Int,Int) -> Vector Int -> a
intCodeLoop io@(IntCodeIO iof) input (ip,rb) mem
  | op == 1 = intCodeLoop io input (ip+4,rb) (write (in1+in2) out3 mem)
  | op == 2 = intCodeLoop io input (ip+4,rb) (write (in1*in2) out3 mem)
  | op == 3 = iof Nothing input (Just (\ newIO newInput -> intCodeLoop newIO (tail newInput) (ip+2,rb) (write (head newInput) out1 mem)))
  | op == 4 = iof (Just in1) input (Just (\ newIO newInput -> intCodeLoop newIO newInput (ip+2,rb) mem))
  | op == 5 = intCodeLoop io input (if in1 /= 0 then in2 else ip+3,rb) mem
  | op == 6 = intCodeLoop io input (if in1 == 0 then in2 else ip+3,rb) mem
  | op == 7 = intCodeLoop io input (ip+4,rb) (write (if in1 < in2 then 1 else 0) out3 mem)
  | op == 8 = intCodeLoop io input (ip+4,rb) (write (if in1 == in2 then 1 else 0) out3 mem)
  | op == 9 = intCodeLoop io input (ip+2,rb+in1) mem
  | op == 99 = iof (Just (mem!0)) input Nothing
  where
    opcode = mem!ip
    op = opcode `mod` 100
    raw1 = mem!(ip+1)
    raw2 = mem!(ip+2)
    raw3 = mem!(ip+3)
    mode1 = (opcode `div` 100) `mod` 10
    mode2 = (opcode `div` 1000) `mod` 10
    mode3 = (opcode `div` 10000) `mod` 10
    in1 | mode1 == 0 = maybe 0 id $ mem!?raw1
        | mode1 == 1 = raw1
        | mode1 == 2 = maybe 0 id $ mem!?(raw1+rb)
    in2 | mode2 == 0 = maybe 0 id $ mem!?raw2
        | mode2 == 1 = raw2
        | mode2 == 2 = maybe 0 id $ mem!?(raw2+rb)
    out1 | mode1 == 0 = raw1
         | mode1 == 2 = raw1+rb
    out3 | mode3 == 0 = raw3
         | mode3 == 2 = raw3+rb
    write val loc vec
      | loc < V.length vec = modify (\ v -> VM.write v loc val) vec
      | otherwise = runST $ do
            mvec1 <- V.thaw vec
            mvec2 <- VM.new (loc + 10000)
            VM.copy (VM.slice 0 (VM.length mvec1) mvec2) mvec1
            VM.write mvec2 loc val
            V.unsafeFreeze mvec2

unsafeIntCode :: [Int] -> Vector Int -> [Int]
unsafeIntCode input = unsafeIntCodeLoop (IntCodeIO io) input (0,0)
  where io val input continuation = maybe [] continue continuation
          where continue f = maybe id (:) val $ f (IntCodeIO io) input

unsafeIntCodeIO :: IntCodeIO a -> [Int] -> Vector Int -> a
unsafeIntCodeIO io input mem = unsafeIntCodeLoop io input (0,0) mem

unsafeIntCodeLoop :: IntCodeIO a -> [Int] -> (Int,Int) -> Vector Int -> a
unsafeIntCodeLoop io@(IntCodeIO iof) input (ip,rb) mem
  | op == 1 = unsafeIntCodeLoop io input (ip+4,rb) (write (in1+in2) out3 mem)
  | op == 2 = unsafeIntCodeLoop io input (ip+4,rb) (write (in1*in2) out3 mem)
  | op == 3 = iof Nothing input (Just (\ newIO newInput -> unsafeIntCodeLoop newIO (tail newInput) (ip+2,rb) (write (head newInput) out1 mem)))
  | op == 4 = iof (Just in1) input (Just (\ newIO newInput -> unsafeIntCodeLoop newIO newInput (ip+2,rb) mem))
  | op == 5 = unsafeIntCodeLoop io input (if in1 /= 0 then in2 else ip+3,rb) mem
  | op == 6 = unsafeIntCodeLoop io input (if in1 == 0 then in2 else ip+3,rb) mem
  | op == 7 = unsafeIntCodeLoop io input (ip+4,rb) (write (if in1 < in2 then 1 else 0) out3 mem)
  | op == 8 = unsafeIntCodeLoop io input (ip+4,rb) (write (if in1 == in2 then 1 else 0) out3 mem)
  | op == 9 = unsafeIntCodeLoop io input (ip+2,rb+in1) mem
  | op == 99 = iof (Just (mem!0)) input Nothing
  where
    opcode = mem!ip
    op = opcode `mod` 100
    raw1 = mem!(ip+1)
    raw2 = mem!(ip+2)
    raw3 = mem!(ip+3)
    mode1 = (opcode `div` 100) `mod` 10
    mode2 = (opcode `div` 1000) `mod` 10
    mode3 = (opcode `div` 10000) `mod` 10
    in1 | mode1 == 0 = maybe 0 id $ mem!?raw1
        | mode1 == 1 = raw1
        | mode1 == 2 = maybe 0 id $ mem!?(raw1+rb)
    in2 | mode2 == 0 = maybe 0 id $ mem!?raw2
        | mode2 == 1 = raw2
        | mode2 == 2 = maybe 0 id $ mem!?(raw2+rb)
    out1 | mode1 == 0 = raw1
         | mode1 == 2 = raw1+rb
    out3 | mode3 == 0 = raw3
         | mode3 == 2 = raw3+rb
    write val loc vec
      | loc < V.length vec = runST $ do
            mvec <- V.unsafeThaw vec
            VM.write mvec loc val
            V.unsafeFreeze mvec
      | otherwise = runST $ do
            mvec1 <- V.unsafeThaw vec
            mvec2 <- VM.new (loc + 10000)
            VM.copy (VM.slice 0 (VM.length mvec1) mvec2) mvec1
            VM.write mvec2 loc val
            V.unsafeFreeze mvec2
