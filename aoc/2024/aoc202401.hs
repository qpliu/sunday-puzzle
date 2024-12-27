module AOC202401 where

import Data.Map(alter,empty)
import qualified Data.Map
import Data.List(sort)

import AOC

aoc = AOC {
    day="01",
    aocTests=[
        AOCTest {
            testData=unlines [
                "3   4",
                "4   3",
                "2   5",
                "1   3",
                "3   9",
                "3   3"
                ],
            testResult=Just "11",
            testResult2=Just "31"
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

parse = p ([],[]) . words
  where
    p (l,r) (a:b:rest) = p (read a:l,read b:r) rest
    p lr [] = lr

result (l,r) = sum $ map abs $ zipWith (-) (sort l) (sort r)

result2 (l,r) = sum $ map score l
  where
    counts = foldr (alter (Just . maybe 1 (+1))) empty r
    count i = maybe 0 id $ Data.Map.lookup i counts
    score i = i*count i
