module AOC202111 where

import Data.Char(ord)
import Data.Map(Map,delete,fromList,insert,member,size,toList,(!))

import AOC

aoc = AOC {
    day="../../2021/input/11",
    aocTests=[
        AOCTest {
            testData=unlines [
                "5483143223",
                "2745854711",
                "5264556173",
                "6141336146",
                "6357385478",
                "4167524645",
                "2176841721",
                "6882881134",
                "4846848554",
                "5283751526"
                ],
            testResult=Just "1656",
            testResult2=Just "195"
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

parse = map (fmap ((flip (-) (ord '0')) . ord)) . toList . parse2d

increase :: ((Int,Int),Int) -> ([(Int,Int)],[((Int,Int),Int)])
         -> ([(Int,Int)],[((Int,Int),Int)])
increase (xy,level) (flashers,unflashed)
  | level >= 9 = (xy:flashers,unflashed)
  | otherwise = (flashers,(xy,level+1):unflashed)

flash :: Int -> ([(Int,Int)],Map (Int,Int) Int,[((Int,Int),Int)])
      -> (Int,[((Int,Int),Int)])
flash flashCount (flashers,unflashed,done)
  | null flashers = (flashCount,done ++ toList unflashed)
  | otherwise = flash (flashCount + length flashers)
                      (foldr flash1 ([],unflashed,done) flashers)
  where
    flash1 xy@(x,y) (flashers,unflashed,done) =
        foldr flash2 (flashers,unflashed,(xy,0):done)
              [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]

    flash2 xy (flashers,unflashed,done)
      | not (member xy unflashed) = (flashers,unflashed,done)
      | level >= 9 = (xy:flashers,delete xy unflashed,done)
      | otherwise = (flashers,insert xy (level+1) unflashed,done)
      where level = unflashed!xy

step :: (Int,[((Int,Int),Int)]) -> (Int,[((Int,Int),Int)])
step (_,grid) = flash 0 (flashers,fromList unflashed,[])
  where (flashers,unflashed) = foldr increase ([],[]) grid

result = sum . map fst . take 101 . iterate step . (,) 0

result2 grid = fst $ head $ dropWhile ((< n) . fst . snd)
                   $ zip [0..] $ iterate step (0,grid)
  where n = length grid
