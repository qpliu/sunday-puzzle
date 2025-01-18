module AOC202024 where

import Data.Set(Set,delete,elems,empty,fromList,insert,
                intersection,member,singleton,size,union,unions)

import AOC

aoc = AOC {
    day="../../2020/input/24",
    aocTests=[
        AOCTest {
            testData=unlines [
                "sesenwnenenewseeswwswswwnenewsewsw",
                "neeenesenwnwwswnenewnwwsewnenwseswesw",
                "seswneswswsenwwnwse",
                "nwnwneseeswswnenewneswwnewseswneseene",
                "swweswneswnenwsewnwneneseenw",
                "eesenwseswswnenwswnwnwsewwnwsene",
                "sewnenenenesenwsewnenwwwse",
                "wenwwweseeeweswwwnwwe",
                "wsweesenenewnwwnwsenewsenwwsesesenwne",
                "neeswseenwwswnwswswnw",
                "nenwswwsewswnenenewsenwsenwnesesenew",
                "enewnwewneswsewnwswenweswnenwsenwsw",
                "sweneswneswneneenwnewenewwneswswnese",
                "swwesenesewenwneswnwwneseswwne",
                "enesenwswwswneneswsenwnewswseenwsese",
                "wnwnesenesenenwwnenwsewesewsesesew",
                "nenewswnwewswnenesenwnesewesw",
                "eneswnwswnwsenenwnwnwwseeswneewsenese",
                "neswnwewnwnwseenwseesewsenwsweewe",
                "wseweeenwnesenwwwswnew"
                ],
            testResult=Just "10",
            testResult2=Just "2208"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=const result,
        pcodeTest2=result2,
        pcodeResult=const result,
        pcodeResult2=result2
        }
    }

follow :: (Int,Int) -> String -> (Int,Int)
follow xy [] = xy
follow (x,y) ('e':rest) = follow (x+1,y) rest
follow (x,y) ('s':'e':rest)
  | even y = follow (x,y+1) rest
  | otherwise = follow (x+1,y+1) rest
follow (x,y) ('s':'w':rest)
  | even y = follow (x-1,y+1) rest
  | otherwise = follow (x,y+1) rest
follow (x,y) ('w':rest) = follow (x-1,y) rest
follow (x,y) ('n':'w':rest)
  | even y = follow (x-1,y-1) rest
  | otherwise = follow (x,y-1) rest
follow (x,y) ('n':'e':rest)
  | even y = follow (x,y-1) rest
  | otherwise = follow (x+1,y-1) rest

flipTile :: (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
flipTile tile floor
  | member tile floor = delete tile floor
  | otherwise = insert tile floor

parse :: String -> Set (Int,Int)
parse = foldr flipTile empty . map (follow (0,0)) . lines 

result = size

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y)
  | even y = [(x+1,y),(x,y+1),(x-1,y+1),(x-1,y),(x-1,y-1),(x,y-1)]
  | otherwise = [(x+1,y),(x+1,y+1),(x,y+1),(x-1,y),(x,y-1),(x+1,y-1)]

step :: Int -> Set (Int,Int) -> Set (Int,Int)
step ncpu floor =
    parallelMapReduce ncpu isBlack unions $ elems $ union floor candidates
  where
    candidates = fromList $ concatMap neighbors $ elems floor
    isBlack xy
      | n == 2 || (n == 1 && member xy floor) = singleton xy
      | otherwise = empty
      where n = size $ intersection floor $ fromList $ neighbors xy

result2 ncpu = size . head . drop 100 . iterate (step ncpu)
