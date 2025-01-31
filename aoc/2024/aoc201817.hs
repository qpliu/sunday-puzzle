module AOC201817 where

import Data.Map(Map,fromList,insert,keys,member,size,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2018/input/17",
    aocTests=[
        AOCTest {
            testData=unlines [
                "x=495, y=2..7",
                "y=7, x=495..501",
                "x=501, y=3..7",
                "x=498, y=2..4",
                "x=506, y=1..2",
                "x=498, y=10..13",
                "x=504, y=10..13",
                "y=13, x=498..504"
                ],
            testResult=Just "57",
            testResult2=Just "29"
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

parse input = (ymin,ymax,ground)
  where
    ymin = minimum $ map snd $ keys ground
    ymax = maximum $ map snd $ keys ground
    ground = fromList $ concatMap p $ lines input
    p (ch:rest)
      | ch == 'x' = [((a,y),'#') | y <- [b..c]]
      | ch == 'y' = [((x,a),'#') | x <- [b..c]]
      where [a,b,c] = parseInts rest

fill :: [(Int,Int)] ->  (Int,Map (Int,Int) Char) -> Map (Int,Int) Char
fill [] (_,ground) = ground
fill path@((x,y):back) (ymax,ground)
  | member (x,y) ground = fill back (ymax,spillLevel (x-1,y) ground)
  | y >= ymax =
      fill back (ymax,insert (x,y) '|' ground)
  | not (member (x,y+1) ground) =
      fill ((x,y+1):path) (ymax,ground)
  | ground!(x,y+1) == '|' =
      fill back (ymax,spillLevel (x-1,y) $ insert (x,y) '|' ground)
  | not (member (x-1,y) ground) =
      fill ((x-1,y):path) (ymax,ground)
  | not (member (x-1,y) ground) =
      fill ((x-1,y):path) (ymax,ground)
  | elem (ground!(x-1,y)) "#?" && not (member (x+1,y) ground) =
      fill ((x+1,y):path) (ymax,insert (x,y) '?' ground)
  | elem (ground!(x-1,y)) "#?" && ground!(x+1,y) == '#' =
      fill back (ymax,fillLevel (x-1,y) $ insert (x,y) '~' ground)
  | not (member (x+1,y) ground) =
      fill ((x+1,y):path) (ymax,insert (x,y) '|' ground)
  | otherwise =
      fill back (ymax,insert (x,y) '|' ground)

fillLevel :: (Int,Int) -> Map (Int,Int) Char -> Map(Int,Int) Char
fillLevel (x,y) ground
  | ground!(x,y) == '?' = fillLevel (x-1,y) (insert (x,y) '~' ground)
  | otherwise = ground

spillLevel :: (Int,Int) -> Map (Int,Int) Char -> Map(Int,Int) Char
spillLevel (x,y) ground
  | not (member (x,y) ground) = ground
  | ground!(x,y) == '?' = spillLevel (x-1,y) (insert (x,y) '|' ground)
  | otherwise = ground

result (ymin,ymax,ground) =
    size $ Data.Map.filter isWater $ fill [(500,ymin)] (ymax,ground)
  where isWater c = c == '|' || c == '~'

result2 (ymin,ymax,ground) =
    size $ Data.Map.filter (== '~') $ fill [(500,ymin)] (ymax,ground)
