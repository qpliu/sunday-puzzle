module AOC201916 where

import Data.Char(chr,isDigit,ord)

import Data.Vector.Unboxed(Vector,fromList,generate,modify,slice,(!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import AOC

aoc = AOC {
    day="../../2019/input/16",
    aocTests=[
        AOCTest {
            testData="80871224585914546619083218645595",
            testResult=Just $ show "24176176",
            testResult2=Nothing
            },
        AOCTest {
            testData="19617804207202209144916044189917",
            testResult=Just $ show "73745418",
            testResult2=Nothing
            },
        AOCTest {
            testData="69317163492948606335995924319873",
            testResult=Just $ show "52432133",
            testResult2=Nothing
            },
        AOCTest {
            testData="03036732577212944063491565474664",
            testResult=Nothing,
            testResult2=Just $ show "84462026"
            },
        AOCTest {
            testData="02935109699940807407585447034323",
            testResult=Nothing,
            testResult2=Just $ show "78725270"
            },
        AOCTest {
            testData="03081770884921959731165446850517",
            testResult=Nothing,
            testResult2=Just $ show "53553731"
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

parse = fromList . map (flip (-) (ord '0') . ord) . filter isDigit

getResult :: Vector Int -> String
getResult list = [chr $ (list!i) + ord '0' | i <- [0..7]]

fft :: Int -> Vector Int -> Vector Int
fft n list
  | n <= 0 = list
  | otherwise = fft (n-1) $ generate len fftE
  where
    len = V.length list
    fftE i = abs (pattern (i+1) i 1 0) `mod` 10
    pattern plen i sign total
      | i >= len = total
      | otherwise =
          pattern plen (i+2*plen) (-sign)
                  (total+sign*(V.sum $ slice i (min plen (len-i)) list))

result = getResult . fft 100

parse2 input = generate (10000*len-offset) initialValue
  where 
    list = parse input
    len = V.length list
    offset = sum [10^(6-i)*(list!i) | i <- [0..6]]
    initialValue i = list!((i+offset) `mod` len)

fft2 :: Int -> Vector Int -> Vector Int
fft2 n list
  | n <= 0 = list
  | otherwise = fft2 (n-1) $ modify (fftE2 (V.length list-1) 0) list
  where
    fftE2 i nextDigit mlist
      | i < 0 = return ()
      | otherwise = do
          digit <- MV.read mlist i
          MV.write mlist i ((digit+nextDigit) `mod` 10)
          fftE2 (i-1) ((digit+nextDigit) `mod` 10) mlist

result2 = getResult . fft2 100
