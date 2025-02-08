module AOC201521 where

import AOC

aoc = AOC {
    day="../../2015/input/21",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Hit Points: 12",
                "Damage: 7",
                "Armor: 2"
                ],
            testResult=Just "90",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 8,
        codeTest2=result2 8,
        codeResult=result 100,
        codeResult2=result2 100
        }
    }

parse = parseInts

weapon :: [(Int,Int)]
weapon = [(8,4),(10,5),(25,6),(40,7),(74,8)]

armor :: [(Int,Int)]
armor = [(0,0),(13,1),(31,2),(53,3),(75,4),(102,5)]

ring :: [(Int,Int,Int)]
ring = [(0,0,0),(25,1,0),(50,2,0),(100,3,0),(20,0,1),(40,0,2),(80,0,3)]

gear :: [(Int,[Int])]
gear = [(wcost+acost+r1cost+r2cost,[wdam+r1dam+r2dam,arm+r1arm+r2arm])
        | (wcost,wdam) <- weapon,
          (acost,arm) <- armor,
          r1@(r1cost,r1dam,r1arm) <- ring,
          r2@(r2cost,r2dam,r2arm) <- ring,
          r1 == (0,0,0) || r1 /= r2]

battle :: [Int] -> [Int] -> Bool
battle [hp1,w1,a1] [hp2,w2,a2] =
    hp1 `div` max 1 (w2-a1) >= hp2 `div` max 1 (w1-a2)

result hp boss =
    minimum [cost | (cost,stats) <- gear, battle (hp:stats) boss]

result2 hp boss =
    maximum [cost | (cost,stats) <- gear, not $ battle (hp:stats) boss]
