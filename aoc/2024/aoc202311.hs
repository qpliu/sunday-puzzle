module AOC202311 where

import Data.Map(Map,fromList,toList,(!))
import Data.Set(Set,empty,insert,member)

import AOC

aoc = AOC {
    day="../../2023/input/11",
    aocTests=[
        AOCTest {
            testData=unlines [
                "...#......",
                ".......#..",
                "#.........",
                "..........",
                "......#...",
                ".#........",
                ".........#",
                "..........",
                ".......#..",
                "#...#....."
                ],
            testResult=Just "374",
            testResult2=Just "8410"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result 2,
        pcodeTest2=result 100,
        pcodeResult=result 2,
        pcodeResult2=result 1000000
        }
    }

parse :: String -> (Set Int,Set Int,[(Int,Int)])
parse = foldr collect (empty,empty,[]) . toList . parse2d
  where
    collect (xy@(x,y),'#') (xs,ys,xys) = (insert x xs,insert y ys,xy:xys)
    collect _ locations = locations

result expansion ncpu (xs,ys,xys) =
    parallelMapReduce ncpu (distance expandX expandY) sum
                      [(xy1,xy2) | xy1 <- xys, xy2 <- xys, xy1 > xy2]
  where
    expandX = expand expansion xs (maximum $ map fst xys)
    expandY = expand expansion ys (maximum $ map snd xys)

expand :: Int -> Set Int -> Int -> Int -> Int
expand expansion nonempty imax = (table!)
  where
    table = fromList $ makeTable 0 0
    makeTable i iexpanded
      | i > imax = []
      | member i nonempty = (i,iexpanded) : makeTable (i+1) (iexpanded+1)
      | otherwise = (i,iexpanded) : makeTable (i+1) (iexpanded+expansion)

distance :: (Int -> Int) -> (Int -> Int) -> ((Int,Int),(Int,Int)) -> Int
distance expandX expandY ((x1,y1),(x2,y2)) =
    abs (expandX x1 - expandX x2) + abs (expandY y1 - expandY y2)
