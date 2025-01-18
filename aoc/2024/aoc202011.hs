module AOC202011 where

import Data.Map(Map,findWithDefault,keys,toList)
import Data.Set(Set,empty,fromList,member,size)

import AOC

aoc = AOC {
    day="../../2020/input/11",
    aocTests=[
        AOCTest {
            testData=unlines [
                "L.LL.LL.LL",
                "LLLLLLL.LL",
                "L.L.L..L..",
                "LLLL.LL.LL",
                "L.LL.LL.LL",
                "L.LLLLL.LL",
                "..L.L.....",
                "LLLLLLLLLL",
                "L.LLLLLL.L",
                "L.LLLLL.LL"
                ],
            testResult=Just "37",
            testResult2=Just "26"
            }
        ],
    aocCode=Code {
        codeParse=parse neighbors1,
        codeParse2=parse neighbors2,
        codeTest=result 4,
        codeTest2=result 5,
        codeResult=result 4,
        codeResult2=result 5
        }
    }

parse neighbors input =
    map (neighbors xmax ymax grid) $ map fst $ filter ((== 'L') . snd)
                                             $ toList grid
  where
    grid = parse2d input
    xmax = maximum $ map fst $ keys grid
    ymax = maximum $ map snd $ keys grid

occupy :: Int -> Set (Int,Int) -> ((Int,Int),[(Int,Int)]) -> Bool
occupy threshold set (xy@(x,y),xys)
  | count == 0 = True
  | count >= threshold = False
  | otherwise = member xy set
  where
    count = length [() | xy <- xys, member xy set]

next :: Int -> [((Int,Int),[(Int,Int)])] -> Set (Int,Int) -> Set (Int,Int)
next threshold seats occupied =
    fromList $ map fst $ filter (occupy threshold occupied) seats

findFixedPoint :: Eq a => [Set a] -> Int
findFixedPoint (a1:rest@(a2:_))
  | a1 == a2 = size a1
  | otherwise = findFixedPoint rest

neighbors1 :: Int -> Int -> Map (Int,Int) Char -> (Int,Int)
           -> ((Int,Int),[(Int,Int)])
neighbors1 _ _ grid xy@(x,y) =
    (xy,[(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1],
                       (dx,dy) /= (0,0),
                       findWithDefault '.' (x+dx,y+dy) grid == 'L'])

neighbors2 :: Int -> Int -> Map (Int,Int) Char -> (Int,Int)
           -> ((Int,Int),[(Int,Int)])
neighbors2 xmax ymax grid xy@(x,y) =
    (xy, take 1 [xy | xy <- zip (repeat x) yplus, seat xy]
      ++ take 1 [xy | xy <- zip (repeat x) yminus, seat xy]
      ++ take 1 [xy | xy <- zip xplus (repeat y), seat xy]
      ++ take 1 [xy | xy <- zip xplus yplus, seat xy]
      ++ take 1 [xy | xy <- zip xplus yminus, seat xy]
      ++ take 1 [xy | xy <- zip xminus (repeat y), seat xy]
      ++ take 1 [xy | xy <- zip xminus yplus, seat xy]
      ++ take 1 [xy | xy <- zip xminus yminus, seat xy])
  where
    seat xy = findWithDefault '.' xy grid == 'L'
    xplus = [x+1,x+2..xmax]
    xminus = [x-1,x-2..0]
    yplus = [y+1,y+2..ymax]
    yminus = [y-1,y-2..0]

result threshold seats = findFixedPoint $ iterate (next threshold seats) empty
