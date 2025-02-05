module AOC201620 where

import AOC

aoc = AOC {
    day="../../2016/input/20",
    aocTests=[
        AOCTest {
            testData=unlines [
                "5-8",
                "0-2",
                "4-7"
                ],
            testResult=Just "3",
            testResult2=Just "2"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2 0 9,
        codeResult=result,
        codeResult2=result2 0 4294967295
        }
    }

parse = foldr merge [] . map (map abs . parseInts) . lines

merge :: [Int] -> [(Int,Int)] -> [(Int,Int)]
merge [imin,imax] [] = [(imin,imax)]
merge [imin,imax] ranges@((rmin,rmax):rest)
  | imax < rmin-1 = (imin,imax):ranges
  | rmax < imin-1 = (rmin,rmax):merge [imin,imax] rest
  | otherwise = merge [min imin rmin,max imax rmax] rest

result ((rmin,rmax):_)
  | rmin > 0 = 0
  | otherwise = rmax+1

result2 imin imax []
  | imin > imax = 0
  | otherwise = imax+1-imin
result2 imin imax ((rmin,rmax):rest)
  | imin > imax = 0
  | imax < rmin = imax+1 - imin
  | otherwise = max 0 (rmin-imin) + result2 (max (rmax+1) imin) imax rest
