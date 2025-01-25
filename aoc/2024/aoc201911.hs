module AOC201911 where

import Data.Map(Map,findWithDefault,insert,singleton,size,toList)
import Data.Set(fromList)

import AOC
import AOC2019

aoc = AOC {
    day="../../2019/input/11",
    aocTests=[],
    aocCode=Code {
        codeParse=parseIntCode,
        codeParse2=parseIntCode,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

type XY = (Int,Int)
type DXY = (Int,Int)

initIO :: Int -> IntCodeIO (Map XY Int)
initIO color = IntCodeIO (paint (0,0) (0,-1) (singleton (0,0) color))

paint :: XY -> DXY -> Map XY Int -> Maybe Int -> [Int]
     -> Maybe (IntCodeIO (Map XY Int) -> [Int] -> Map XY Int)
     -> Map XY Int
paint xy dxy painted output input = maybe painted continue
  where
    Just color = output
    continue cont
      | output == Nothing = cont (IntCodeIO $ paint xy dxy painted) input
      | otherwise = cont (IntCodeIO nextIO) [color]
    nextIO = turn xy dxy (insert xy color painted)

turn :: XY -> DXY -> Map XY Int -> Maybe Int -> [Int]
     -> Maybe (IntCodeIO (Map XY Int) -> [Int] -> Map XY Int)
     -> Map XY Int
turn xy@(x,y) dxy@(dx,dy) painted dir input = maybe painted continue
  where
    continue cont
      | dir == Nothing = cont (IntCodeIO $ turn xy dxy painted) input
      | otherwise = cont (IntCodeIO nextIO) [findWithDefault 0 newXY painted]
    newXY = (x+newDX,y+newDY)
    newDXY@(newDX,newDY) | dir == Just 0 = (dy,-dx) | otherwise = (-dy,dx)
    nextIO = paint newXY newDXY painted

result = size . intCodeIO (initIO 0) [0]

result2 = ocr4x6 . toImg . intCodeIO (initIO 1) [1]
  where
    toImg = show2ds False . fromList . map fst . filter ((== 1) . snd) . toList
