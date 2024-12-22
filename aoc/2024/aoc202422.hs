module AOC202422 where

import Data.Bits(xor,shiftL,shiftR,(.&.))
import Data.Map(Map,empty,member,insert,unionsWith)

import AOC

aoc = AOC {
    day="22",
    testData=unlines [
    "1",
    "10",
    "100",
    "2024"
    ],
    testResult="37327623",
    testData2=unlines ["1","2","3","2024"],
    testResult2="23",
    aocParse=parse,
    aocTest=result,
    aocResult=result,
    aocParse2=parse,
    aocTest2=result2,
    aocResult2=result2
    }

parse :: String -> [Int]
parse = map read . words

next :: Int -> Int
next x = x3
  where
    x1 = ((x `shiftL` 6) `xor` x) .&. 16777215
    x2 = ((x1 `shiftR` 5) `xor` x1) .&. 16777215
    x3 = ((x2 `shiftL`  11) `xor` x2) .&. 16777215

nextN :: Int -> Int -> Int
nextN n = head . drop n . iterate next

result = sum . map (nextN 2000)

gen :: (Int,(Int,Int,Int,Int),Int) -> (Int,(Int,Int,Int,Int),Int)
gen (secret,(dp1,dp2,dp3,dp4),price) =
    (nsecret,(dp2,dp3,dp4,nprice-price),nprice)
  where
    nsecret = next secret
    nprice = nsecret `mod` 10

collect :: Map (Int,Int,Int,Int) Int -> (Int,(Int,Int,Int,Int),Int) -> Map (Int,Int,Int,Int) Int
collect dict (_,seq,price)
  | member seq dict = dict
  | otherwise = insert seq price dict

prices :: Int -> Map (Int,Int,Int,Int) Int
prices secret = foldl collect empty
        $ drop 4 $ take 2001 $ iterate gen (secret,(0,0,0,0),0)

result2 = maximum . unionsWith (+) . map prices
