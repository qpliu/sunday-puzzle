module AOC202422 where

import Data.Bits(xor,shiftL,shiftR,(.&.),(.|.))
import Data.IntMap(IntMap,fromListWith,unionsWith)

import AOC

aoc = AOC {
    day="22",
    aocTests=[
        AOCTest {
            testData=unlines [
                "1",
                "10",
                "100",
                "2024"
                ],
            testResult=Just "37327623",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines ["1","2","3","2024"],
            testResult=Nothing,
            testResult2=Just "23"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

parse :: String -> [Int]
parse = map read . words

next :: Int -> Int
next x0 = x3
  where
    x1 = ((x0 `shiftL` 6)  `xor` x0) .&. 0xffffff
    x2 = ((x1 `shiftR` 5)  `xor` x1)
    x3 = ((x2 `shiftL` 11) `xor` x2) .&. 0xffffff

nextN :: Int -> Int -> Int
nextN n = head . drop n . iterate next

result ncpu = parallelMapReduce ncpu (nextN 2000) sum

gen :: ((Int,Int),Int) -> ((Int,Int),Int)
gen ((changes,price),secret) = ((nchanges,nprice),nsecret)
  where
    nsecret = next secret
    nprice = nsecret `mod` 10
    nchanges =
        ((changes .&. 0x3fff) `shiftL` 5) .|. ((price - nprice) .&. 0x1f)

prices :: Int -> IntMap Int
prices = fromListWith (curry fst) . map fst . drop 4 . take 2001
                                  . iterate gen . (,) (0,0)

result2 ncpu = maximum . parallelMapReduce ncpu prices (unionsWith (+))
