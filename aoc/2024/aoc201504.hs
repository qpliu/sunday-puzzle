module AOC201504 where

import Crypto.Hash.MD5(hash)
import Data.Bits((.&.))
import Data.ByteString(ByteString,index,pack)
import qualified Data.ByteString
import Data.Char(ord)
import Data.Word(Word8)

import AOC

aoc = AOC {
    day="../../2015/input/04",
    aocTests=[
        AOCTest {
            testData=unlines [
                "abcdef"
                ],
            testResult=Just "609043",
            testResult2=Nothing
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

parse :: String -> [Word8]
parse = map (fromIntegral . ord) . filter (/= '\n')

md5 :: [Word8] -> Int -> (Int,ByteString)
md5 key i = (i,hash $ pack $ key ++ map (fromIntegral . ord) (show i))

md5s :: [Word8] -> [(Int,ByteString)]
md5s key = map (md5 key) [1..]

lacks5 :: (Int,ByteString) -> Bool
lacks5 (_,b) = index b 0 > 0 || index b 1 > 0 || index b 2 > 15

result = fst . head . dropWhile lacks5 . md5s

lacks6 :: (Int,ByteString) -> Bool
lacks6 (_,b) = index b 0 > 0 || index b 1 > 0 || index b 2 > 0

result2 = fst . head . dropWhile lacks6 . md5s
