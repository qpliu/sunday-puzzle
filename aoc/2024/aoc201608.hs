module AOC201608 where

import Data.Set(Set,empty,fromList,size,union)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2016/input/08",
    aocTests=[],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

makeScreen :: Set (Int,Int) -> [String] -> Set (Int,Int)
makeScreen pixels ["rect",axb] = union pixels $ fromList rect
  where
    [a,b] = parseInts axb
    rect = [(x,y) | x <- [0..a-1], y <- [0..b-1]]
makeScreen pixels ["rotate","row",as,"by",bs] = Data.Set.map rot pixels
  where
    [a] = parseInts as
    [b] = parseInts bs
    rot xy@(x,y)
      | y == a = ((x+b) `mod` 50,y)
      | otherwise = xy
makeScreen pixels ["rotate","column",as,"by",bs] = Data.Set.map rot pixels
  where
    [a] = parseInts as
    [b] = parseInts bs
    rot xy@(x,y)
      | x == a = (x,(y+b) `mod` 6)
      | otherwise = xy

parse = foldl makeScreen empty . map words . lines

result = size

result2 = ocr4x6 . show2ds False
