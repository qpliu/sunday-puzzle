module AOC201617 where

import Crypto.Hash.MD5(hash)
import Data.Bits((.&.))
import Data.ByteString(ByteString,index,pack,snoc,unpack)
import qualified Data.ByteString
import Data.Char(chr,ord)
import Data.Word(Word8)

import AOC

aoc = AOC {
    day="../../2016/input/17",
    aocTests=[
        AOCTest {
            testData="ihgpwlah",
            testResult=Just $ show "DDRRRD",
            testResult2=Just "370"
            },
        AOCTest {
            testData="kglvqrro",
            testResult=Just $ show "DDUDRLRRUDRD",
            testResult2=Just "492"
            },
        AOCTest {
            testData="ulqzkmiv",
            testResult=Just $ show "DRURDRUDDLLDLUURRDULRLDUUDDDRR",
            testResult2=Just "830"
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

parse = map (fromIntegral . ord) . filter (/= '\n')

walk :: [((Int,Int),ByteString)] -> [((Int,Int),ByteString)] -> [ByteString]
walk [] [] = []
walk nextQueue [] = walk [] nextQueue
walk nextQueue (((x,y),path):queue)
  | (x,y) == (4,4) = path : walk nextQueue queue
  | otherwise = walk enqueued queue
  where
    doors = hash path
    enqueued = enqueueU
    enqueueU
      | y > 1 && index doors 0 >= 0xb0 = ((x,y-1),snoc path 85):enqueueD
      | otherwise = enqueueD
    enqueueD
      | y < 4 && index doors 0 .&. 15 > 10 = ((x,y+1),snoc path 68):enqueueL
      | otherwise = enqueueL
    enqueueL
      | x > 1 && index doors 1 >= 0xb0 = ((x-1,y),snoc path 76):enqueueR
      | otherwise = enqueueR
    enqueueR
      | x < 4 && index doors 1 .&. 15 > 10 = ((x+1,y),snoc path 82):nextQueue
      | otherwise = nextQueue

result passcode =
    map (chr . fromIntegral) $ drop (length passcode) $ unpack $ head
                             $ walk [] [((1,1),pack passcode)]

result2 passcode =
    (+ (-length passcode)) $ Data.ByteString.length $ last
                           $ walk [] [((1,1),pack passcode)]
