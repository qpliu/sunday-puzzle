module AOC201720 where

import Data.Map(Map,alter,empty,member,(!))

import AOC

aoc = AOC {
    day="../../2017/input/20",
    aocTests=[
        AOCTest {
            testData=unlines [
                "p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>",
                "p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"
                ],
            testResult=Just "0",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>",
                "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>",
                "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>",
                "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"
                ],
            testResult=Nothing,
            testResult2=Just "1"
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

parse = map parseInts . lines

result = snd . minimum . map metric . zip [0..]
  where
    metric (i,[x,y,z,vx,vy,vz,ax,ay,az]) =
      ((abs ax+abs ay+abs az,abs vx+abs vy+abs vz,abs x+abs y+abs z),i)

update :: Int -> [[Int]] -> [[Int]]
update ncpu = parallelMap ncpu updateParticle

updateParticle :: [Int] -> [Int]
updateParticle [x,y,z,vx,vy,vz,ax,ay,az] =
    [x+vx+ax,y+vy+ay,z+vz+az,vx+ax,vy+ay,vz+az,ax,ay,az]

escape :: Int -> ([((Int,Int,Int),[Int])] -> ((Int,Int,Int),[Int]))
       -> Int -> [[Int]] -> Maybe [[Int]]
escape ncpu _ _ [] = Nothing
escape ncpu _ _ [_] = Just []
escape ncpu extreme index particles
  | null particles || a == 0 = Nothing
  | extremeX == extremeV && extremeV == extremeA =
      Just $ filter (/= extremeX) particles
  | otherwise = Nothing
  where
    (_,extremeX) = extreme $ map addX particles
    (_,extremeV) = extreme $ map addV particles
    ((a,_,_),extremeA) = extreme $ map addA particles
    addX particle =
        ((particle!!index,0,0),particle)
    addV particle =
        ((particle!!(index+3),particle!!index,0),particle)
    addA particle =
        ((particle!!(index+6),particle!!(index+3),particle!!index),particle)

escapes :: Int -> (Int,[[Int]]) -> (Int,[[Int]])
escapes ncpu (count,particles) =
    try maximum 0 $ try minimum 0 $ try maximum 1 $ try minimum 1
                  $ try maximum 2 $ try minimum 2 (count,particles)
  where
    try extreme index tryNext =
        maybe tryNext ok $ escape ncpu extreme index particles
    ok = escapes ncpu . (,) (count+1)

collide :: [[Int]] -> [[Int]]
collide particles = filter collided particles
  where
    collided (x:y:z:_) = positions!(x,y,z) < 2
    positions = foldr collect empty particles
    collect (x:y:z:_) = alter (Just . maybe 1 (1+)) (x,y,z)

evolve :: Int -> Int -> [[Int]] -> Int
evolve ncpu count [] = count
evolve ncpu count particles = evolve ncpu (count+escapeCount) nextParticles
  where
    (escapeCount,nextParticles) =
        escapes ncpu $ (,) 0 $ collide $ update ncpu particles

result2 ncpu = evolve ncpu 0
