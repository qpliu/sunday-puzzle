module AOC201614 where

import Crypto.Hash.MD5(hash)
import Data.Bits(setBit,shiftR,testBit,(.&.))
import Data.ByteString(ByteString,unpack,pack)
import Data.Char(ord)
import Data.Maybe(catMaybes)
import Data.Word(Word8)

import AOC

aoc = AOC {
    day="../../2016/input/14",
    aocTests=[
        AOCTest {
            testData="abc",
            testResult=Just "22728",
            testResult2=Nothing --Just "22859"
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

parse :: String -> [Word8]
parse = map (fromIntegral . ord) . filter (/= '\n')

nybbles :: ByteString -> [Word8]
nybbles = concatMap toNybbles . unpack
  where toNybbles b = [shiftR b 4,b .&. 15]

md5 :: [Word8] -> Int -> ByteString
md5 salt = hash . pack . (salt++) . map (fromIntegral . ord) . show

triplet :: Int -> [Word8] -> Maybe ((Int,Int),Int)
triplet index = snd . foldl collect ((0,16),Nothing)
  where
    collect ((n,digit),Nothing) d
      | digit /= d = ((1,d),Nothing)
      | n < 2 = ((n+1,d),Nothing)
      | otherwise = ((n+1,d),Just ((index,fromIntegral d),0))
    collect ((n,digit),r@(Just (d3,d5s))) d
      | digit /= d = ((1,d),r)
      | n == 4 = ((5,d),Just (d3,setBit d5s (fromIntegral d)))
      | otherwise = ((n+1,d),r)

indexes :: Int -> (ByteString -> ByteString) -> [Word8] -> [Int]
indexes ncpu stretch salt = filterKeys $ catMaybes $ block 0
  where
    block start = parallelMap ncpu calculate [start .. start+100*ncpu-1]
                      ++ block (start+100*ncpu)

    calculate i = triplet i $ nybbles $ stretch $ md5 salt i
    filterKeys (((i,d3),_):rest)
      | any ((`testBit` d3) . snd) $ takeWhile ((<= (i+1000)) . fst . fst) rest =
          i : filterKeys rest
      | otherwise = filterKeys rest

result ncpu = head . drop 63 . indexes ncpu id

hex :: Word8 -> Word8
hex 0 = 48
hex 1 = 49
hex 2 = 50
hex 3 = 51
hex 4 = 52
hex 5 = 53
hex 6 = 54
hex 7 = 55
hex 8 = 56
hex 9 = 57
hex 10 = 97
hex 11 = 98
hex 12 = 99
hex 13 = 100
hex 14 = 101
hex 15 = 102

stretch2016 :: ByteString -> ByteString
stretch2016 = head . drop 2016 . iterate (hash . pack . map hex . nybbles)

result2 ncpu = head . drop 63 . indexes ncpu stretch2016
