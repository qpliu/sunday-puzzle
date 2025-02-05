module AOC201616 where

import AOC

aoc = AOC {
    day="../../2016/input/16",
    aocTests=[
        AOCTest {
            testData=unlines [
                "10000"
                ],
            testResult=Just $ show "01100",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 20,
        codeTest2=undefined,
        codeResult=result 272,
        codeResult2=result 35651584
        }
    }

parse = filter (/= '\n')

curve :: String -> String
curve s = s ++ ('0':map inv (reverse s))
  where
    inv '0' = '1'
    inv '1' = '0'

checksum :: String -> String
checksum (a:b:rest)
  | a == b = '1':checksum rest
  | otherwise = '0':checksum rest
checksum [] = []

result len state = head $ drop iters $ iterate checksum $ take len
                 $ head $ drop iters $ iterate curve state
  where
    iters = log2 (len `div` length state)

log2 :: Int -> Int
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)
