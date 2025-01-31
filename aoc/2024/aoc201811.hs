module AOC201811 where

import Data.Array(Array,array,assocs,bounds,(!))

import AOC

aoc = AOC {
    day="../../2018/input/11",
    aocTests=[
        AOCTest {
            testData="18",
            testResult=Just "(33,45)",
            testResult2=Just "(90,269,16)"
            },
        AOCTest {
            testData="42",
            testResult=Just "(21,61)",
            testResult2=Just "(232,251,12)"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

parse = makeGrid . read
  where
    makeGrid :: Int -> Array (Int,Int) Int
    makeGrid serial =
        array ((1,1),(300,300))
              [((x,y),power serial x y) | x <- [1..300], y <- [1..300]]

    power :: Int -> Int -> Int -> Int
    power serial  x y =
        ((((x + 10)*y + serial)*(x + 10)) `div` 100) `mod` 10 - 5

powerPatch :: Array (Int,Int) Int -> Int -> Int -> Int
powerPatch grid x y = grid!(x,y)  +grid!(x+1,y)  +grid!(x+2,y)
                     +grid!(x,y+1)+grid!(x+1,y+1)+grid!(x+2,y+1)
                     +grid!(x,y+2)+grid!(x+1,y+2)+grid!(x+2,y+2)

result ncpu grid =
    snd $ parallelMapReduce ncpu id maximum
            [(powerPatch grid x y,(x,y)) | x <- [1..298],y <- [1..298]]

searchBySize :: Int -> Array (Int,Int) Int -> Array (Int,Int) (Int,(Int,Int,Int)) -> Int -> (Int,(Int,Int,Int))
searchBySize ncpu grid lastPatch patchSize
  | patchSize >= 300 || lastPower < 0 = lastMax
  | otherwise = max lastMax (searchBySize ncpu grid patch (patchSize+1))
  where
    lastMax@(lastPower,_) = maximum lastPatch
    patch = array ((1,1),(300-patchSize,300-patchSize))
                $ parallelMap ncpu patchPower
                            [(x,y) | x <- [1..300-patchSize],
                                     y <- [1..300-patchSize]]

    patchPower (x,y) =
        ((x,y),(fst (lastPatch!(x+1,y+1)) + grid!(x,y)
                + sum (map edges [1..patchSize]),(x,y,patchSize+1)))
      where edges dx = grid!(x+dx,y) + grid!(x,y+dx)

result2 ncpu grid = snd $ searchBySize ncpu grid patch 1
  where
    patch = array (bounds grid) $ map toPatch $ assocs grid
    toPatch ((x,y),power) = ((x,y),(power,(x,y,1)))
