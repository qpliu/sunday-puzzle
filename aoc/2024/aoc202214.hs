module AOC202214 where

import Data.Set(Set,elems,fromList,member,insert,size)

import AOC

aoc = AOC {
    day="../../2022/input/14",
    aocTests=[
        AOCTest {
            testData=unlines [
                "498,4 -> 498,6 -> 496,6",
                "503,4 -> 502,4 -> 502,9 -> 494,9"
                ],
            testResult=Just "24",
            testResult2=Just "93"
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

parse input = (ymax,cave)
  where
    cave = fromList $ concatMap (toRock . parseInts) $ lines input
    toRock [x1,y1] = [(x1,y1)]
    toRock (x1:y1:rest@(x2:y2:_))
      | x1 == x2 = [(x1,y) | y <- [min y1 y2..max y1 y2]] ++ toRock rest
      | y1 == y2 = [(x,y1) | x <- [min x1 x2..max x1 x2]] ++ toRock rest
    ymax = maximum $ map snd $ elems cave

source :: (Int,Int)
source = (500,0)

pour :: Int -> Set (Int,Int) -> [(Int,Int)] -> Set (Int,Int)
pour ymax cave track@(xy@(x,y):backtrack)
  | y >= ymax = cave
  | not (member (x,y+1) cave) = pour ymax cave ((x,y+1):track)
  | not (member (x-1,y+1) cave) = pour ymax cave ((x-1,y+1):track)
  | not (member (x+1,y+1) cave) = pour ymax cave ((x+1,y+1):track)
  | otherwise = pour ymax (insert xy cave) backtrack

result (ymax,cave) = flip (-) (size cave) $ size $ pour ymax cave [source]

pour2 :: Int -> Set (Int,Int) -> [(Int,Int)] -> Set (Int,Int)
pour2 ymax cave [] = cave
pour2 ymax cave track@(xy@(x,y):backtrack)
  | y > ymax = pour2 ymax (insert xy cave) backtrack
  | not (member (x,y+1) cave) = pour2 ymax cave ((x,y+1):track)
  | not (member (x-1,y+1) cave) = pour2 ymax cave ((x-1,y+1):track)
  | not (member (x+1,y+1) cave) = pour2 ymax cave ((x+1,y+1):track)
  | otherwise = pour2 ymax (insert xy cave) backtrack

result2 (ymax,cave) = flip (-) (size cave) $ size $ pour2 ymax cave [source]
