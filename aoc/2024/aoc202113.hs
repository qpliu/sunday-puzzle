module AOC202113 where

import Data.Set(Set,fromList,size)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2021/input/13",
    aocTests=[
        AOCTest {
            testData=unlines [
                "6,10",
                "0,14",
                "9,10",
                "0,3",
                "10,4",
                "4,11",
                "6,0",
                "6,12",
                "4,1",
                "0,13",
                "10,12",
                "3,4",
                "3,0",
                "8,4",
                "1,10",
                "2,14",
                "8,10",
                "9,0",
                "",
                "fold along y=7",
                "fold along x=5"
                ],
            testResult=Just "17",
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

parse :: String -> (Set (Int,Int),[Either Int Int])
parse = p . span (not . null) . lines
  where
    p (points,_:folds) = (fromList $ map (parsePoint . parseInts) points,
                          map (parseFold . words) folds)
    parsePoint [x,y] = (x,y)
    parseFold [_,_,'x':'=':x] = Left $ read x
    parseFold [_,_,'y':'=':y] = Right $ read y

fold :: Set (Int,Int) -> Either Int Int -> Set (Int,Int)
fold points (Left xFold) = Data.Set.map f points
  where
    f (x,y) | x <= xFold = (x,y)
            | otherwise = (2*xFold - x,y)
fold points (Right yFold) = Data.Set.map f points
  where
    f (x,y) | y <= yFold = (x,y)
            | otherwise = (x,2*yFold - y)

result (points,(instruction:_)) = size $ fold points instruction

result2 (points,instructions) =
    ocr4x6 $ unlines $ drop 1 $ lines $ show2ds
           $ foldl fold points instructions
