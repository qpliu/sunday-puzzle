module AOC201724 where

import Data.List(partition)
import Data.Tuple(swap)

import AOC

aoc = AOC {
    day="../../2017/input/24",
    aocTests=[
        AOCTest {
            testData=unlines [
                "0/2",
                "2/2",
                "2/3",
                "3/4",
                "3/5",
                "0/1",
                "10/1",
                "9/10"
                ],
            testResult=Just "31",
            testResult2=Just "19"
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

parse = map parseInts . lines

build :: ((Int,Int) -> (Int,Int)) -> Int -> (Int,Int) -> [[Int]] -> (Int,Int)
build metric port (len,stren) components
  | null compatible = metric (len,stren)
  | otherwise =
      maximum [build metric (getPort component)
                     (len+1,stren+sum component)
                     (rest++incompatible)
               | (component,rest) <- selectEach [] compatible]
  where
    (compatible,incompatible) = partition (elem port) components

    selectEach _ [] = []
    selectEach selected (item:items) =
        (item,selected++items) : selectEach (item:selected) items

    getPort [a,b] | a == port = b | otherwise = a

result = fst . build swap 0 (0,0)

result2 = snd . build id 0 (0,0)
