module AOC201912 where

import AOC

aoc = AOC {
    day="../../2019/input/12",
    aocTests=[
        AOCTest {
            testData=unlines [
                "<x=-1, y=0, z=2>",
                "<x=2, y=-10, z=-7>",
                "<x=4, y=-8, z=8>",
                "<x=3, y=5, z=-1>"
                ],
            testResult=Nothing,
            testResult2=Just "2772"
            },
        AOCTest {
            testData=unlines [
                "<x=-8, y=-10, z=0>",
                "<x=5, y=5, z=10>",
                "<x=2, y=-7, z=3>",
                "<x=9, y=-8, z=-3>"
                ],
            testResult=Just "1940",
            testResult2=Just "4686774924"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=const (result 100),
        pcodeTest2=result2,
        pcodeResult=const (result 1000),
        pcodeResult2=result2
        }
    }

parse = map (((,) [0,0,0]) . parseInts) . lines

energy :: [([Int],[Int])] -> Int
energy = sum . map e
  where e (v,x) = sum (map abs v) * sum (map abs x)

gravity :: [([Int],[Int])] -> [([Int],[Int])]
gravity moons = map g moons
  where
    g (v,x) = (zipWith (+) dv v,x)
      where
        dv = foldr g2 (repeat 0) moons
        g2 (_,x2) dv = zipWith (+) dv (map signum $ zipWith (-) x2 x)

velocity :: [([Int],[Int])] -> [([Int],[Int])]
velocity = map vel
  where vel (v,x) = (v,zipWith (+) v x)

result nsteps = energy . head . drop nsteps . iterate (velocity . gravity)

gravity2 :: [(Int,Int)] -> [(Int,Int)]
gravity2 [(v1,x1),(v2,x2),(v3,x3),(v4,x4)] =
    [(v1+signum (x2-x1)+signum (x3-x1)+signum (x4-x1),x1),
     (v2+signum (x1-x2)+signum (x3-x2)+signum (x4-x2),x2),
     (v3+signum (x1-x3)+signum (x2-x3)+signum (x4-x3),x3),
     (v4+signum (x1-x4)+signum (x2-x4)+signum (x3-x4),x4)]

velocity2 :: [(Int,Int)] -> [(Int,Int)]
velocity2 [(v1,x1),(v2,x2),(v3,x3),(v4,x4)] =
    [(v1,x1+v1),(v2,x2+v2),(v3,x3+v3),(v4,x4+v4)]

recurrence :: Eq a => (a -> a) -> a -> Int
recurrence step a =
    fst $ head $ dropWhile ((/= a) . snd) $ zip [1..] $ tail $ iterate step a

result2 ncpu moons =
    foldr lcm 1 $ parallelMap ncpu (recurrence (velocity2 . gravity2)) moon1s
  where moon1s = [map (((,) 0) . head . drop i . snd) moons | i <- [0,1,2]]
