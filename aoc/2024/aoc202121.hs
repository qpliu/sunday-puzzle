module AOC202121 where

import Data.Map(Map,fromList,(!))
import Data.Tuple(swap)

import AOC

aoc = AOC {
    day="../../2021/input/21",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Player 1 starting position: 4",
                "Player 2 starting position: 8"
                ],
            testResult=Just "739785",
            testResult2=Just "444356092776315"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse = p . parseInts
  where p [_,p1,_,p2] = (p1,0,p2,0)

turn :: (Int,Int,(Int,Int,Int,Int)) -> (Int,Int,(Int,Int,Int,Int))
turn (die,turns,(p1,s1,p2,s2)) = (newDie,turns+1,(p2,s2,newP1,s1+newP1))
  where
    newDie = (die-1+3) `mod` 1000 + 1
    newP1 = (p1-1+3*die) `mod` 10 + 1

end :: (Int,Int,(Int,Int,Int,Int)) -> Maybe Int
end (_,turns,(_,s1,_,s2))
  | s2 >= 1000 = Just $ turns*3*s1
  | otherwise = Nothing

play :: (Int,Int,(Int,Int,Int,Int)) -> Int
play state = maybe (play $ turn state) id $ end state

result players = play (2,0,players)

-- 21*21*10*10 = 48400 states,
-- scores are 0..20, positions are 1 .. 10

-- 1 universe: move 3 (111)
-- 3 universes: move 4 (112,121,211)
-- 6 universes: move 5 (113,131,311,122,212,221)
-- 7 universes: move 6 (222,123,132,213,231,312,321)
-- 6 universes: move 7 (331,313,133,322,232,223)
-- 3 universes: move 8 (332,323,233)
-- 1 universe: move 9 (333)

result2 = uncurry max . (dirac!)
  where
    dirac = fromList [(k,wins k)
                      | p1 <- [1..10], p2 <- [1..10],
                        s1 <- [0..20], s2 <- [0..20],
                        k <- [(p1,s1,p2,s2)]]
    mult n (win,lose) = (n*win,n*lose)
    plus (win1,lose1) (win2,lose2) = (win1+win2,lose1+lose2)
    sumWins = foldr plus (0,0)
    wins (p1,s1,p2,s2) =
        sumWins [diracTurn rolls
                 | rolls <- [(1,3),(3,4),(6,5),(7,6),(6,7),(3,8),(1,9)]]
      where
        diracTurn (nUniverses,moves)
          | nextP1+s1 >= 21 = (nUniverses,0)
          | otherwise = mult nUniverses $ swap $ dirac!(p2,s2,nextP1,nextP1+s1)
          where nextP1 = (p1-1+moves) `mod` 10 + 1
