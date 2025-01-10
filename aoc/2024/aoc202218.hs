module AOC202218 where

import Data.Map(Map,empty,insert)
import qualified Data.Map
import Data.Set(Set,elems,fromList,member)

import AOC

aoc = AOC {
    day="../../2022/input/18",
    aocTests=[
        AOCTest {
            testData=unlines [
                "1,1,1",
                "2,1,1"
                ],
            testResult=Just "10",
            testResult2=Just "10"
            },
        AOCTest {
            testData=unlines [
                "2,2,2",
                "1,2,2",
                "3,2,2",
                "2,1,2",
                "2,3,2",
                "2,2,1",
                "2,2,3",
                "2,2,4",
                "2,2,6",
                "1,2,5",
                "3,2,5",
                "2,1,5",
                "2,3,5"
                ],
            testResult=Just "64",
            testResult2=Just "58"
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

parse = fromList . map parseInts . lines

result ncpu lava =
    parallelMapReduce ncpu (countExposedFaces lava) sum (elems lava)

countExposedFaces :: Set [Int] -> [Int] -> Int
countExposedFaces lava xyz =
    length [() | axyz <- adjacent xyz,
                 not $ member axyz lava]

adjacent :: [Int] -> [[Int]]
adjacent [x,y,z] =
    [[x+1,y,z],[x-1,y,z],[x,y+1,z],[x,y-1,z],[x,y,z+1],[x,y,z-1]]

result2 ncpu lava = sum $ fillExterior lava

fillExterior :: Set [Int] -> Map [Int] Int
fillExterior lava = fill empty [[xmin,ymin,zmin]]
  where
    xmin = -1 + minimum (map head $ elems lava)
    xmax =  1 + maximum (map head $ elems lava)
    ymin = -1 + minimum (map (head . drop 1) $ elems lava)
    ymax =  1 + maximum (map (head . drop 1) $ elems lava)
    zmin = -1 + minimum (map (head . drop 2) $ elems lava)
    zmax =  1 + maximum (map (head . drop 2) $ elems lava)

    inBounds [x,y,z] = x >= xmin && x <= xmax && y >= ymin && y <= ymax
                                              && z >= zmin && z <= zmax

    fill exterior [] = exterior
    fill exterior (xyz:queue)
      | Data.Map.member xyz exterior = fill exterior queue
      | otherwise = fill (insert xyz faces exterior) (enqueue ++ queue)
      where
        faces = length [() | axyz <- adjacent xyz, member axyz lava]
        enqueue = [axyz | axyz <- adjacent xyz,
                          inBounds axyz,
                          not $ Data.Map.member axyz exterior,
                          not $ member axyz lava]
