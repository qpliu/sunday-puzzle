module AOC201915 where

import Data.Map(Map,empty,insert,member,toList,(!))
import Data.Vector.Unboxed(Vector)

import AOC
import AOC2019

aoc = AOC {
    day="../../2019/input/15",
    aocTests=[],
    aocCode=Code {
        codeParse=parseIntCode,
        codeParse2=parseIntCode,
        codeTest=undefined,
        codeTest2=undefined,
        codeResult=result,
        codeResult2=result2
        }
    }

type XY = (Int,Int)

data Bot = Bot (Int -> (Int,Bot))

bfs :: Int -> Map XY Int -> [(XY,Bot)] -> Int
bfs nsteps area locs = step [] area locs
  where
    step nextSteps area [] = bfs (nsteps+1) area nextSteps
    step nextSteps area ((xy@(x,y),Bot bot):bots)
      | member xy area = step nextSteps area bots
      | otherwise = stepDir nextSteps (insert xy 1 area)
                            [(1,(x,y-1)),(2,(x,y+1)),(3,(x-1,y)),(4,(x+1,y))]
      where
        stepDir nextSteps area [] = step nextSteps area bots
        stepDir nextSteps area ((dir,nextXY):dirs)
          | member nextXY area = stepDir nextSteps area dirs
          | botResult == 0 = stepDir nextSteps (insert nextXY 0 area) dirs
          | botResult == 1 = stepDir ((nextXY,nextBot):nextSteps) area dirs
          | botResult == 2 = nsteps + 1
          where (botResult,nextBot) = bot dir

initBot :: Vector Int -> Int -> (Int,Bot)
initBot mem dir = intCodeIO (IntCodeIO continueBot) [dir] mem
  where
    continueBot output input (Just cont)
      | output == Nothing = cont (IntCodeIO continueBot) input
      | otherwise = (botResult,Bot continue)
      where
        Just botResult = output
        continue dir = cont (IntCodeIO continueBot) [dir]

result mem = bfs 0 empty [((0,0),Bot (initBot mem))]

mapArea :: Map XY Int -> [(XY,Int,Bot)] -> Map XY Int
mapArea area [] = area
mapArea area ((xy@(x,y),locType,Bot bot):bots)
  | member xy area = mapArea area bots
  | otherwise = mapDir (insert xy locType area) bots
                       [(1,(x,y-1)),(2,(x,y+1)),(3,(x-1,y)),(4,(x+1,y))]
  where
    mapDir area bots [] = mapArea area bots
    mapDir area bots ((dir,nextXY):dirs)
      | member nextXY area = mapDir area bots dirs
      | botResult == 0 = mapDir (insert nextXY 0 area) bots dirs
      | otherwise = mapDir area ((nextXY,botResult,nextBot):bots) dirs
      where (botResult,nextBot) = bot dir

fill :: Int -> (Map XY Int,[XY]) -> Int
fill time (area,[]) = time - 1
fill time (area,locs) = fill (time+1) $ foldr fill1 (area,[]) locs
  where
    fill1 loc@(x,y) (area,nextLocs)
      | area!loc == 3 = (area,nextLocs)
      | otherwise = (insert loc 3 area,newLocs ++ nextLocs)
      where newLocs = [xy | xy <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)],
                            member xy area, area!xy == 1]

result2 mem = fill 0 (area,map fst $ filter ((== 2) . snd) $ toList area)
  where area = mapArea empty [((0,0),1,Bot (initBot mem))]
