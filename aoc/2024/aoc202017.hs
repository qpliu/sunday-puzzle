module AOC202017 where

import Control.Monad.Par(NFData)
import Data.Map(toList)
import qualified Data.Map
import Data.Set(Set,elems,fromList,intersection,member,size,unions)

import AOC

aoc = AOC {
    day="../../2020/input/17",
    aocTests=[
        AOCTest {
            testData=unlines [
                ".#.",
                "..#",
                "###"
                ],
            testResult=Just "112",
            testResult2=Just "848"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse2,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

parse = fromList . map toXYZ . filter ((== '#') . snd) . toList . parse2d
  where toXYZ ((x,y),_) = (x,y,0)

step :: (NFData a, Ord a) => Int -> (a -> Set a) -> Set a -> Set a
step ncpu neighbors set = fromList nextPoints
  where
    points = parallelMapReduce ncpu neighbors unions $ elems set
    nextPoints = parallelMapReduce ncpu life concat $ elems points
    life point
      | n == 3 = [point]
      | n == 2 && member point set = [point]
      | otherwise = []
      where n = size $ intersection set $ neighbors point

neighbors3 :: (Int,Int,Int) -> Set (Int,Int,Int)
neighbors3 (x,y,z) =
    fromList [(x+dx,y+dy,z+dz)
              | dx <- [-1,0,1], dy <- [-1,0,1], dz <- [-1,0,1],
                (dx,dy,dz) /= (0,0,0)]

result ncpu = size . head . drop 6 . iterate (step ncpu neighbors3)

parse2 = fromList . map toXYZW . filter ((== '#') . snd) . toList . parse2d
  where toXYZW ((x,y),_) = (x,y,0,0)

neighbors4 :: (Int,Int,Int,Int) -> Set (Int,Int,Int,Int)
neighbors4 (x,y,z,w) =
    fromList [(x+dx,y+dy,z+dz,w+dw)
              | dx <- [-1,0,1], dy <- [-1,0,1], dz <- [-1,0,1], dw <- [-1,0,1],
                (dx,dy,dz,dw) /= (0,0,0,0)]

result2 ncpu = size . head . drop 6 . iterate (step ncpu neighbors4)
