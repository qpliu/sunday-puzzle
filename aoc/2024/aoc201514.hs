module AOC201514 where

import AOC

aoc = AOC {
    day="../../2015/input/14",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.",
                "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
                ],
            testResult=Just "1120",
            testResult2=Just "689"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 1000,
        codeTest2=result2 1000,
        codeResult=result 2503,
        codeResult2=result2 2503
        }
    }

parse = map parseInts . lines

distAt :: Int -> [Int] -> Int
distAt t [speed,dt1,dt2] = n*speed*dt1 + speed*min dt1 dt
  where (n,dt) = divMod t (dt1+dt2)

result t = maximum . map (distAt t)

dists :: Int -> [Int] -> [Int]
dists d0 stats@[speed,dt1,dt2] =
    [d0 + t*speed | t <- [1..dt1]]
        ++ take dt2 (repeat (d0+dt1*speed))
        ++ dists (d0 + dt1*speed) stats

scores :: [Int] -> [[Int]] -> [[Int]]
scores currentScores currentDists =
    currentScores : scores nextScores (map tail currentDists)
  where
    maxDist = head $ maximum currentDists
    nextScores = zipWith addPoint currentScores currentDists
    addPoint currentScore (currentDist:_)
      | currentDist < maxDist = currentScore
      | otherwise = currentScore+1

result2 t = maximum . head . drop t . scores (repeat 0) . map (dists 0)
