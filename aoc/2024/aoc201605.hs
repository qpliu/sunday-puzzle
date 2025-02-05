module AOC201605 where

import Crypto.Hash.MD5(hash)
import Data.ByteString(ByteString,index,pack)
import Data.Char(ord)
import Data.Map(Map,elems,empty,insert,member,size)
import Data.Word(Word8)

import AOC

aoc = AOC {
    day="../../2016/input/05",
    aocTests=[
        AOCTest {
            testData=unlines [
                "abc"
                ],
            testResult=Just $ show "18f47a30",
            testResult2=Just $ show "05ace8e3"
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

hex :: Word8 -> Char
hex 0 = '0'
hex 1 = '1'
hex 2 = '2'
hex 3 = '3'
hex 4 = '4'
hex 5 = '5'
hex 6 = '6'
hex 7 = '7'
hex 8 = '8'
hex 9 = '9'
hex 10 = 'a'
hex 11 = 'b'
hex 12 = 'c'
hex 13 = 'd'
hex 14 = 'e'
hex 15 = 'f'

hashes :: Int -> Word8 -> Int -> [Word8] -> [ByteString]
hashes ncpu max6th n doorID =
    parallelMapReduce ncpu generate concat [n..n+1000*ncpu-1]
        ++ hashes ncpu max6th (n+1000*ncpu-1) doorID
  where
    generate i
      | index h 0 == 0 && index h 1 == 0 && index h 2 < max6th = [h]
      | otherwise = []
      where h = hash $ pack $ doorID ++ map (fromIntegral . ord) (show i)

result ncpu = map (hex . (`index` 2)) . take 8 . hashes ncpu 16 0

collect :: Map Word8 Char -> [ByteString] -> String
collect pwd (h:hs)
  | member (index h 2) pwd = collect pwd hs
  | size pwd < 7 = collect nextPwd hs
  | otherwise = elems nextPwd
  where nextPwd = insert (index h 2) (hex (index h 3 `div` 16)) pwd

result2 ncpu = collect empty . hashes ncpu 8 0
