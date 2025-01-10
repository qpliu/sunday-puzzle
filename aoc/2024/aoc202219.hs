module AOC202219 where

import Data.Set(Set,fromList,insert,maxView)

import AOC

aoc = AOC {
    day="../../2022/input/19",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.",
                "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."
                ],
            testResult=Just "33",
            testResult2=Just (show (56*62))
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

parse = map parseInts . lines

-- Looking at the input data, it doesn't make sense to have more than 4
-- ore robots, and probably no more than 2.  The maximum number of ore
-- robots should be the maximum of the number of ore required for each
-- of the other types of robots.
-- If I passed on building a robot that I have enough resources to make,
-- making that robot should not be an option until after a different robot
-- is built.

type State = ((Int,Int,Bool),(Int,Int,Bool),(Int,Int,Bool),(Int,Int,Bool),Int)

production :: Int -> [Int] -> Int
production timeLimit [_,oreOre,clyOre,obsOre,obsCly,geoOre,geoObs] = finalGeo
  where
    finalState :: State
    finalState@((finalGeo,_,_),_,_,_,_) =
        search $ fromList
               $ [project ((0,0,False),(0,0,False),(0,0,False),(0,1,False),0)]

    project :: State -> (Int,State)
    project state@((geo,geobots,_),_,_,_,time) =
        (geo+dt*geobots+(dt*(dt+1))`div`2,state)
      where dt = timeLimit-time

    maxOreBots = maximum [oreOre,clyOre,obsOre,geoOre]

    search :: Set (Int,State) -> State
    search queue
      | time == timeLimit = state
      | otherwise = search $ foldr insert queue1 $ map project $ nextSteps
      where
        Just ((_,state@((geo,geoBots,geoPass),(obs,obsBots,obsPass),
                        (cly,clyBots,clyPass),(ore,oreBots,orePass),
                        time)),queue1) =
            maxView queue
        nextSteps = makeGeoBot $ makeObsBot $ makeClyBot $ makeOreBot $ wait

        makeGeoBot
          | not geoPass && obs >= geoObs && ore >= geoOre =
              (((geo+geoBots,geoBots+1,False),
                (obs+obsBots-geoObs,obsBots,False),
                (cly+clyBots,clyBots,False),
                (ore+oreBots-geoOre,oreBots,False),time+1):)
          | otherwise = id

        makeObsBot
          | not obsPass && cly >= obsCly && ore >= obsOre && obsBots < geoObs =
              (((geo+geoBots,geoBots,False),
                (obs+obsBots,obsBots+1,False),
                (cly+clyBots-obsCly,clyBots,False),
                (ore+oreBots-obsOre,oreBots,False),time+1):)
          | otherwise = id

        makeClyBot
          | not clyPass && ore >= clyOre && clyBots < obsCly =
              (((geo+geoBots,geoBots,False),
                (obs+obsBots,obsBots,False),
                (cly+clyBots,clyBots+1,False),
                (ore+oreBots-clyOre,oreBots,False),time+1):)
          | otherwise = id

        makeOreBot
          | not orePass && ore >= oreOre && oreBots < maxOreBots =
              (((geo+geoBots,geoBots,False),
                (obs+obsBots,obsBots,False),
                (cly+clyBots,clyBots,False),
                (ore+oreBots-oreOre,oreBots+1,False),time+1):)
          | otherwise = id

        wait =
              [((geo+geoBots,geoBots,ore >= geoOre && obs >= geoObs),
                (obs+obsBots,obsBots,ore >= obsOre && cly >= obsCly),
                (cly+clyBots,clyBots,ore >= clyOre),
                (ore+oreBots,oreBots,ore >= oreOre),time+1)]

qualityLevel :: [Int] -> Int
qualityLevel blueprint@(idNumber:_) = idNumber*production 24 blueprint

result ncpu = parallelMapReduce ncpu qualityLevel sum

result2 ncpu = parallelMapReduce ncpu (production 32) product . take 3
