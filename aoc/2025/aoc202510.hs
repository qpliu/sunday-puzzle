module AOC202510 where

import Data.Bits(shiftL,xor,(.|.))
import Data.List(nub)
import Data.Map(Map,adjust,fromList,toList,(!))
import qualified Data.Map
import Data.Set(empty,insert,member)

import AOC

aoc = AOC {
    day="10",
    aocTests=[
        AOCTest {
            testData=unlines [
                "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}",
                "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}",
                "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
            ],
            testResult=Just "7",
            testResult2=Just "33"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse :: String -> [(Int,[Int],[Int])]
parse = map (p . words) . lines
  where
    p (lights:rest) =
        (plights 0 1 lights,map pswitch (init rest),parseInts (last rest))
    plights bits bit ('[':rest) = plights bits bit rest
    plights bits bit (']':rest) = bits
    plights bits bit ('#':rest) = plights (bits .|. bit) (shiftL bit 1) rest
    plights bits bit ('.':rest) = plights bits (shiftL bit 1) rest
    pswitch = sum . map (shiftL 1) . parseInts

result = sum . map countPresses

countPresses (0,_,_) = 0
countPresses (goal,switches,_) = count 1 switches [] empty
  where
    count npresses [] queue seen = count (npresses+1) queue [] seen
    count npresses (r:rest) queue seen
      | r == goal = npresses
      | member r seen = count npresses rest queue seen
      | otherwise =
          count npresses rest (map (xor r) switches ++ queue) (insert r seen)

{- way too slow code:

parse2 = map (p . drop 1 . words) . lines
  where p spec = (map parseInts (init spec),fromList $ zip [0..] $ parseInts (last spec))

result2 = map countPresses2

countPresses2 (switches,goal) =
    minimum $ tryPresses 0 (Data.Map.map (const 0) goal) switches
  where
    maxPresses state switch = minimum [goal!s - state!s | s <- switch]
    tryPresses npresses state []
      | state == goal = [npresses]
      | otherwise = []
    tryPresses npresses state (switch:switches) =
        concat [tryPresses (npresses+n) (foldr (adjust (+n)) state switch)
                           switches
                | n <- [0 .. maxPresses state switch]]
-}

parse2 = map (p . drop 1 . words) . lines
  where p spec = (map parseInts (init spec),parseInts (last spec))

result2 = undefined

buildMatrix (switches,goal) =
    nub [[if elem i switch then 1 else 0 | switch <- switches] ++ [g]
         | (i,g) <- zip [0..] goal]
{-
   In general, the number of unknowns is not equal to the number of equations.

   In my input, for every case where there are more joltages than switches,
   it results in duplicate rows in the matrix, so there are never more
   equations than unknowns.

   Additional constraints: unknowns must be non-negative integers.

   https://en.wikipedia.org/wiki/Linear_programming
-}
