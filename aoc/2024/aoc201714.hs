module AOC201714 where

import Data.Bits(popCount,testBit)
import Data.Set(Set,difference,empty,fromList,insert,member)
import Data.Vector.Unboxed(Vector,(!))
import qualified Data.Vector.Unboxed as V

import AOC
import AOC2017

aoc = AOC {
    day="../../2017/input/14",
    aocTests=[
        AOCTest {
            testData="flqrgnkx",
            testResult=Just "8108",
            testResult2=Just "1242"
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

knots :: String -> [Vector Int]
knots key = [knot (key++"-"++show row) | row <- [0..127]]

parse = knots . filter (/= '\n')

result = sum . map (V.foldl (+) 0 . V.map popCount)

groups :: Set (Int,Int) -> Int
groups set
  | null set = 0
  | otherwise = 1 + groups (difference set group)
  where
    group = walk [minimum set] empty
    walk [] g = g
    walk (xy@(x,y):queue) g
      | member xy g = walk queue g
      | otherwise = walk (nexts++queue) (insert xy g)
      where nexts = [nextXY | nextXY <- [(x+1,y),(x,y+1),(x-1,y),(x,y-1)],
                              member nextXY set, not (member nextXY g)]

toXY :: Int -> Vector Int -> [(Int,Int)]
toXY y v = [(x,y) | x <- [0..8*V.length v-1],
                    testBit (v!(x`div`8)) (7 - x`mod`8)]

result2 = groups . fromList . concat . zipWith toXY [0..]
