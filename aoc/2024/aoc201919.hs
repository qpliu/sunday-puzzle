module AOC201919 where

import Data.Map(fromList)
import Data.Vector.Unboxed(Vector)

import AOC
import AOC2019

aoc = AOC {
    day="../../2019/input/19",
    aocTests=[],
    aocCode=Code {
        codeParse=parseIntCode,
        codeParse2=parseIntCode,
        codeTest=undefined,
        codeTest2=undefined,
        codeResult=result,
        codeResult2=result2
        }
    }

queryDrone :: Vector Int -> [Int] -> Int
queryDrone mem xy = head $ unsafeIntCode xy mem

result mem = sum $ map (queryDrone mem) [[x,y] | x <- [0..49], y <- [0..49]]

search :: ([Int] -> Int) -> Int
search query = search1 100
  where
    search1 i
      | not (null resultsX) = search2 xx xy
      | not (null resultsY) = search2 yx yy
      | otherwise = search1 (i+100)
      where
        xstrip = [((x,i),query [x,i]) | x <- [0,100..i]]
        ystrip = [((i,y),query [i,y]) | y <- [0,100..i]]
        resultsX = filter hasWidth (zip xstrip (drop 1 xstrip))
        resultsY = filter hasWidth (zip ystrip (drop 1 ystrip))
        hasWidth ((_,a),(_,b)) = a == 1 && b == 1

        ((((xx,xy),1),_):_) = resultsX
        ((((yx,yy),1),_):_) = resultsY

    search2 x y
      | xp && yp = search3 x y
      | xp = search2 (x+100) y
      | yp = search2 x (y+100)
      | query [x+50,y+50] == 1 = search2 (x+50) (y+50)
      | query [x+25,y+25] == 1 = search2 (x+25) (y+25)
      | query [x+13,y+13] == 1 = search2 (x+13) (y+13)
      | query [x+7,y+7] == 1 = search2 (x+7) (y+7)
      | query [x+4,y+4] == 1 = search2 (x+4) (y+4)
      | query [x+2,y+2] == 1 = search2 (x+2) (y+2)
      | query [x+1,y+1] == 1 = search2 (x+1) (y+1)
      where
        xp = query [x+100,y] == 1
        yp = query [x,y+100] == 1

    search3 x y
      | query [x-100,y-100] == 1 = search3 (x-100) (y-100)
      | otherwise = search4xy x y 50

    search4xy x y dxy
      | query [x-dxy,y-dxy] == 1 && query [x-dxy+99,y-dxy] == 1
                                 && query [x-dxy,y-dxy+99] == 1 =
          search4xy (x-dxy) (y-dxy) dxy
      | dxy > 1 = search4xy x y (dxy `div` 2)
      | (x,y) == (x1,y1) = search5 x y
      | otherwise = search4xy x1 y1 100
      where (x1,y1) = search4x x y 100

    search4x x y dx
      | query [x-dx,y] == 1 && query [x-dx,y+99] == 1 =
          search4x (x-dx) y dx
      | dx > 1 = search4x x y (dx `div` 2)
      | (x,y) == (x1,y1) = (x,y)
      | otherwise = search4x x1 y1 100
      where (x1,y1) = search4y x y 100

    search4y x y dy
      | query [x,y-dy] == 1 && query [x+99,y-dy] == 1 =
          search4y x (y-dy) dy
      | dy > 1 = search4y x y (dy `div` 2)
      | otherwise = (x,y)

    search5 x y = snd $ minimum $ map probe [[x-dx,y-dy] | dx <- [0..49],
                                                           dy <- [0..49]]
      where
        probe [x,y] =
           ((3-query [x,y]-query [x+99,y]-query [x,y+99],x+y), 10000*x+y)

result2 = search . queryDrone
