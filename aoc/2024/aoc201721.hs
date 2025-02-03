module AOC201721 where

import Data.Bits(popCount,setBit,testBit)
import Data.Map(Map,alter,empty,member,fromList,toList,(!))
import Data.Tuple(swap)

import AOC

aoc = AOC {
    day="../../2017/input/21",
    aocTests=[
        AOCTest {
            testData=unlines [
                "../.# => ##./#../...",
                ".#./..#/### => #..#/..../..../#..#"
                ],
            testResult=Just "12",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 2,
        codeTest2=undefined,
        codeResult=result 5,
        codeResult2=result 18
        }
    }

bits :: [Int] -> Int
bits = foldl setBit 0

start :: [(Int,Int)]
start = [(1,bits [21,32,13,23,33])]

rot2 :: Int -> Int
rot2 grid = bits [dest | (src,dest) <- [(11,21),(21,22),(12,11),(22,12)],
                         testBit grid src]

flip2 :: Int -> Int
flip2 grid = bits [dest | (src,dest) <- [(11,12),(12,11),(21,22),(22,21)],
                          testBit grid src]

rot3 :: Int -> Int
rot3 grid = bits [dest | (src,dest) <- [(11,31),(21,32),(31,33),
                                        (12,21),(22,22),(32,23),
                                        (13,11),(23,12),(33,13)],
                         testBit grid src]

flip3 :: Int -> Int
flip3 grid = bits [dest | (src,dest) <- [(11,31),(21,21),(31,11),
                                         (12,32),(22,22),(32,12),
                                         (13,33),(23,23),(33,13)],
                          testBit grid src]

parseRule2 :: String -> [(Int,Int)]
parseRule2 (s11:s12:'/':s21:s22:' ':'=':'>':' '
           :d11:d12:d13:'/':d21:d22:d23:'/':d31:d32:d33:_) =
    [(src,dest),(rot2 src,dest),
     (rot2 $ rot2 src,dest),(rot2 $ rot2 $ rot2 src,dest),
     (flip2 src,dest),(flip2 $ rot2 src,dest),
     (flip2 $ rot2 $ rot2 src,dest),(flip2 $ rot2 $ rot2 $ rot2 src,dest)]
  where
    src = bits [b | (b,s) <- [(11,s11),(12,s12),(21,s21),(22,s22)],
                    s == '#']
    dest = bits [b | (b,d) <- [(11,d11),(21,d21),(31,d31),
                               (12,d12),(22,d22),(32,d32),
                               (13,d13),(23,d23),(33,d33)],
                     d == '#']
parseRule2 _ = []

parseRule3 :: String -> [(Int,[Int])]
parseRule3 (s11:s12:s13:'/':s21:s22:s23:'/':s31:s32:s33:' ':'=':'>':' '
           :d11:d12:d13:d14:'/':d21:d22:d23:d24:'/'
           :d31:d32:d33:d34:'/':d41:d42:d43:d44:_) =
    [(src,dest),(rot3 src,dest),
     (rot3 $ rot3 src,dest),(rot3 $ rot3 $ rot3 src,dest),
     (flip3 src,dest),(flip3 $ rot3 src,dest),
     (flip3 $ rot3 $ rot3 src,dest),(flip3 $ rot3 $ rot3 $ rot3 src,dest)]
  where
    src = bits [b | (b,s) <- [(11,s11),(21,s21),(31,s31),
                              (12,s12),(22,s22),(32,s32),
                              (13,s13),(23,s23),(33,s33)],
                    s == '#']
    dest = [bits [b | (b,d) <- [(11,d11),(21,d21),(12,d12),(22,d22)],
                     d == '#'],
            bits [b | (b,d) <- [(11,d31),(21,d41),(12,d32),(22,d42)],
                     d == '#'],
            bits [b | (b,d) <- [(11,d13),(21,d23),(12,d14),(22,d24)],
                     d == '#'],
            bits [b | (b,d) <- [(11,d33),(21,d43),(12,d34),(22,d44)],
                     d == '#']]
parseRule3 _ = []

parse input = (rules2,rules3)
  where
    rules2 = fromList $ concatMap parseRule2 $ lines input
    rules3 = fromList $ concatMap parseRule3 $ lines input

enhance3 :: [(Int,Int)] -> (Map Int Int,Map Int [Int]) -> [Int]
enhance3 grids rules@(_,rules3) =
    sum (map (uncurry (*) . fmap popCount) grids)
        : enhance4 (map eh3 grids) rules
  where
    eh3 = fmap (rules3!)

enhance4 :: [(Int,[Int])] -> (Map Int Int,Map Int [Int]) -> [Int]
enhance4 grids rules@(rules2,_) =
    sum (map (uncurry (*) . fmap (sum . map popCount)) grids)
        : enhance6 (map eh4 grids) rules
  where
    eh4 = fmap (map (rules2!))

enhance6 :: [(Int,[Int])] -> (Map Int Int,Map Int [Int]) -> [Int]
enhance6 grids rules@(rules2,_) =
    sum (map (uncurry (*) . fmap (sum . map popCount)) grids)
        : enhance3 nextGrids rules
  where
    nextGrids = map swap $ toList $ foldr collect empty $ concatMap eh6 grids
    collect (n,grid) = alter (Just . maybe n (n+)) grid
    eh6 (n,[nw,ne,sw,se]) =
        [(n,rules2!grid11),(n,rules2!grid21),(n,rules2!grid31),
         (n,rules2!grid12),(n,rules2!grid22),(n,rules2!grid32),
         (n,rules2!grid13),(n,rules2!grid23),(n,rules2!grid33)]
      where
        grid11 = bits [b | (b,t) <- [(11,testBit nw 11),(21,testBit nw 21),
                                     (12,testBit nw 12),(22,testBit nw 22)], t]
        grid21 = bits [b | (b,t) <- [(11,testBit nw 31),(21,testBit ne 11),
                                     (12,testBit nw 32),(22,testBit ne 12)], t]
        grid31 = bits [b | (b,t) <- [(11,testBit ne 21),(21,testBit ne 31),
                                     (12,testBit ne 22),(22,testBit ne 32)], t]
        grid12 = bits [b | (b,t) <- [(11,testBit nw 13),(21,testBit nw 23),
                                     (12,testBit sw 11),(22,testBit sw 21)], t]
        grid22 = bits [b | (b,t) <- [(11,testBit nw 33),(21,testBit ne 13),
                                     (12,testBit sw 31),(22,testBit se 11)], t]
        grid32 = bits [b | (b,t) <- [(11,testBit ne 23),(21,testBit ne 33),
                                     (12,testBit se 21),(22,testBit se 31)], t]
        grid13 = bits [b | (b,t) <- [(11,testBit sw 12),(21,testBit sw 22),
                                     (12,testBit sw 13),(22,testBit sw 23)], t]
        grid23 = bits [b | (b,t) <- [(11,testBit sw 32),(21,testBit se 12),
                                     (12,testBit sw 33),(22,testBit se 13)], t]
        grid33 = bits [b | (b,t) <- [(11,testBit se 22),(21,testBit se 32),
                                     (12,testBit se 23),(22,testBit se 33)], t]

result n = head . drop n . enhance3 start

debug3 :: (Int,Int) -> String
debug3 (n,grid) = show n ++ ":\n" ++ show2dm False (fromList [
    ((1,1),if testBit grid 11 then '#' else '.'),
    ((2,1),if testBit grid 21 then '#' else '.'),
    ((3,1),if testBit grid 31 then '#' else '.'),
    ((1,2),if testBit grid 12 then '#' else '.'),
    ((2,2),if testBit grid 22 then '#' else '.'),
    ((3,2),if testBit grid 32 then '#' else '.'),
    ((1,3),if testBit grid 13 then '#' else '.'),
    ((2,3),if testBit grid 23 then '#' else '.'),
    ((3,3),if testBit grid 33 then '#' else '.')
    ])

debug4 :: (Int,[Int]) -> String
debug4 (n,[nw,ne,sw,se]) = show n ++ ":\n" ++ show2dm False (fromList [
    ((1,1),if testBit nw 11 then '#' else '.'),
    ((2,1),if testBit nw 21 then '#' else '.'),
    ((3,1),'|'),
    ((4,1),if testBit ne 11 then '#' else '.'),
    ((5,1),if testBit ne 21 then '#' else '.'),
    ((1,2),if testBit nw 12 then '#' else '.'),
    ((2,2),if testBit nw 22 then '#' else '.'),
    ((3,2),'|'),
    ((4,2),if testBit ne 12 then '#' else '.'),
    ((5,2),if testBit ne 22 then '#' else '.'),
    ((1,3),'-'),
    ((2,3),'-'),
    ((3,3),'+'),
    ((4,3),'-'),
    ((5,3),'-'),
    ((1,4),if testBit sw 11 then '#' else '.'),
    ((2,4),if testBit sw 21 then '#' else '.'),
    ((3,4),'|'),
    ((4,4),if testBit se 11 then '#' else '.'),
    ((5,4),if testBit se 21 then '#' else '.'),
    ((1,5),if testBit sw 12 then '#' else '.'),
    ((2,5),if testBit sw 22 then '#' else '.'),
    ((3,5),'|'),
    ((4,5),if testBit se 12 then '#' else '.'),
    ((5,5),if testBit se 22 then '#' else '.')
    ])


debug6 :: (Int,[Int]) -> String
debug6 (n,[nw,ne,sw,se]) = show n ++ ":\n" ++ show2dm False (fromList [
    ((1,1),if testBit nw 11 then '#' else '.'),
    ((2,1),if testBit nw 21 then '#' else '.'),
    ((3,1),if testBit nw 31 then '#' else '.'),
    ((4,1),'|'),
    ((5,1),if testBit ne 11 then '#' else '.'),
    ((6,1),if testBit ne 21 then '#' else '.'),
    ((7,1),if testBit ne 31 then '#' else '.'),
    ((1,2),if testBit nw 12 then '#' else '.'),
    ((2,2),if testBit nw 22 then '#' else '.'),
    ((3,2),if testBit nw 22 then '#' else '.'),
    ((4,2),'|'),
    ((5,2),if testBit ne 12 then '#' else '.'),
    ((6,2),if testBit ne 22 then '#' else '.'),
    ((7,2),if testBit ne 22 then '#' else '.'),
    ((1,3),if testBit nw 13 then '#' else '.'),
    ((2,3),if testBit nw 23 then '#' else '.'),
    ((3,3),if testBit nw 23 then '#' else '.'),
    ((4,3),'|'),
    ((5,3),if testBit ne 13 then '#' else '.'),
    ((6,3),if testBit ne 23 then '#' else '.'),
    ((7,3),if testBit ne 23 then '#' else '.'),
    ((1,4),'-'),
    ((2,4),'-'),
    ((3,4),'-'),
    ((4,4),'+'),
    ((5,4),'-'),
    ((6,4),'-'),
    ((7,4),'-'),
    ((1,5),if testBit sw 11 then '#' else '.'),
    ((2,5),if testBit sw 21 then '#' else '.'),
    ((3,5),if testBit sw 31 then '#' else '.'),
    ((4,5),'|'),
    ((5,5),if testBit se 11 then '#' else '.'),
    ((6,5),if testBit se 21 then '#' else '.'),
    ((7,5),if testBit se 31 then '#' else '.'),
    ((1,6),if testBit sw 12 then '#' else '.'),
    ((2,6),if testBit sw 22 then '#' else '.'),
    ((3,6),if testBit sw 32 then '#' else '.'),
    ((4,6),'|'),
    ((5,6),if testBit se 12 then '#' else '.'),
    ((6,6),if testBit se 22 then '#' else '.'),
    ((7,6),if testBit se 32 then '#' else '.'),
    ((1,7),if testBit sw 13 then '#' else '.'),
    ((2,7),if testBit sw 23 then '#' else '.'),
    ((3,7),if testBit sw 33 then '#' else '.'),
    ((4,7),'|'),
    ((5,7),if testBit se 13 then '#' else '.'),
    ((6,7),if testBit se 23 then '#' else '.'),
    ((7,7),if testBit se 33 then '#' else '.')
    ])
