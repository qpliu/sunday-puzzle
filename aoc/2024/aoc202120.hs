module AOC202120 where

import Data.Array(Array,array,(!))
import Data.Map(keysSet)
import qualified Data.Map
import Data.Set(Set,elems,fromList,member,size)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2021/input/20",
    aocTests=[
        AOCTest {
            testData=unlines [
                "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##" ++
                "#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###" ++
                ".######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#." ++
                ".#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#....." ++
                ".#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.." ++
                "...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#....." ++
                "..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#",
                "",
                "#..#.",
                "#....",
                "##..#",
                "..#..",
                "..###"
                ],
            testResult=Just "35",
            testResult2=Just "3351"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result 2,
        pcodeTest2=result 50,
        pcodeResult=result 2 ,
        pcodeResult2=result 50
        }
    }

parse = p . lines
  where
    p (iea:"":image) = (toIEA iea,(False,toImage (unlines image)))
    toIEA = array (0,511) . zip [0..] . map (== '#')
    toImage = keysSet . Data.Map.filter (== '#') . parse2d

enhance :: Int -> Array Int Bool
        -> (Bool,Set (Int,Int)) -> (Bool,Set (Int,Int))
enhance ncpu iea (wasInverted,image) =
    (inverted,fromList $ parallelMapReduce ncpu pixel concat xys)
  where
    inverted = wasInverted /= iea!0
    xs = Data.Set.map fst image
    ys = Data.Set.map snd image
    xys = [(x,y) | x <- [minimum xs-1 .. maximum xs+1],
                   y <- [minimum ys-1 .. maximum ys+1]]
    pixel xy
      | iea!(index xy) /= inverted = [xy]
      | otherwise = []
    index (x,y) =
        sum [bit
             | (bit,xy) <- [(256,(x-1,y-1)),(128,(x,y-1)),(64,(x+1,y-1)),
                            (32, (x-1,y)),  (16, (x,y)),  (8, (x+1,y)),
                            (4,  (x-1,y+1)),(2,  (x,y+1)),(1, (x+1,y+1))],
               member xy image /= wasInverted]

result n ncpu (iea,image) =
    size $ snd $ head $ drop n $ iterate (enhance ncpu iea) image
