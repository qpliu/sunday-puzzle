module AOC201910 where

import Data.Array(Array,assocs,(!))
import Data.List(sort)

import AOC

aoc = AOC {
    day="../../2019/input/10",
    aocTests=[
        AOCTest {
            testData=unlines [
                ".#..#",
                ".....",
                "#####",
                "....#",
                "...##"
                ],
            testResult=Just "8",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "......#.#.",
                "#..#.#....",
                "..#######.",
                ".#.#.###..",
                ".#..#.....",
                "..#....#.#",
                "#..#....#.",
                ".##.#..###",
                "##...#..#.",
                ".#....####"
                ],
            testResult=Just "33",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "#.#...#.#.",
                ".###....#.",
                ".#....#...",
                "##.#.#.#.#",
                "....#.#.#.",
                ".##..###.#",
                "..#...##..",
                "..##....##",
                "......#...",
                ".####.###."
                ],
            testResult=Just "35",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                ".#..#..###",
                "####.###.#",
                "....###.#.",
                "..###.##.#",
                "##.##.#.#.",
                "....###..#",
                "..#.#..#.#",
                "#..#.#.###",
                ".##...##.#",
                ".....#.#.."
                ],
            testResult=Just "41",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                ".#..##.###...#######",
                "##.############..##.",
                ".#.######.########.#",
                ".###.#######.####.#.",
                "#####.##.#.##.###.##",
                "..#####..#.#########",
                "####################",
                "#.####....###.#.#.##",
                "##.#################",
                "#####.##.###..####..",
                "..######..##.#######",
                "####.##.####...##..#",
                ".#####..#.######.###",
                "##...#.##########...",
                "#.##########.#######",
                ".####.#.###.###.#.##",
                "....##.##.###..#####",
                ".#.#.###########.###",
                "#.#.#.#####.####.###",
                "###.##.####.##.#..##"
                ],
            testResult=Just "210",
            testResult2=Just "802"
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

parse input = (grid,asteroids)
  where
    grid = parse2da input
    asteroids = map fst $ filter ((== '#') . snd) $ assocs grid

count :: Array (Int,Int) Char -> [(Int,Int)] -> (Int,Int) -> (Int,(Int,Int))
count grid asteroids xy@(x,y) = (length $ filter visible asteroids,xy)
  where
    visible xya@(xa,ya)
      | xya == xy = False
      | d == 1 = True
      | otherwise = 1 == length [() | i <- [1 .. n],
                                      grid!(x+i*dx,y+i*dy) == '#']
      where
        d = gcd (abs (xa-x)) (abs (ya-y))
        (dx,dy) = ((xa-x) `div` d,(ya-y) `div` d)
        n | dx == 0 = abs (ya-y) `div` abs dy
          | otherwise = abs (xa-x) `div` abs dx

findAsteroid :: Int -> (Array (Int,Int) Char,[(Int,Int)]) -> (Int,(Int,Int))
findAsteroid ncpu (grid,asteroids) =
    parallelMapReduce ncpu (count grid asteroids) maximum asteroids

result ncpu = fst . findAsteroid ncpu

vaporize :: [(Int,Int)] -> (Int,Int) -> [Int]
vaporize asteroids (x,y) =
    start $ sort $ map addAngle $ filter (/= (x,y)) asteroids
  where
    addAngle :: (Int,Int) -> ((Int,Rational),Int,Int)
    addAngle (ax,ay)
      | ax >= x && ay < y =
          ((0,fromIntegral (ax-x)/fromIntegral (y-ay)),(ax-x)+(y-ay),ax*100+ay)
      | ax > x && ay >= y =
          ((1,fromIntegral (ay-y)/fromIntegral (ax-x)),(ax-x)+(ay-y),ax*100+ay)
      | ax <= x && ay > y =
          ((2,fromIntegral (x-ax)/fromIntegral (ay-y)),(x-ax)+(ay-y),ax*100+ay)
      | ax < x && ay <= y =
          ((3,fromIntegral (y-ay)/fromIntegral (x-ax)),(x-ax)+(y-ay),ax*100+ay)

    start :: [((Int,Rational),Int,Int)] -> [Int]
    start [] = []
    start ((angle,_,loc):rest) = loc : sweep angle rest []

    sweep :: (Int,Rational) -> [((Int,Rational),Int,Int)]
                            -> [((Int,Rational),Int,Int)] -> [Int]
    sweep angle [] remaining = start $ reverse remaining
    sweep angle (asteroid@(angle1,_,loc):rest) remaining
      | angle == angle1 = sweep angle rest (asteroid:remaining)
      | otherwise = loc : sweep angle1 rest remaining

result2 ncpu (grid,asteroids) =
    (!!199) $ vaporize asteroids $ snd $ findAsteroid ncpu (grid,asteroids)
