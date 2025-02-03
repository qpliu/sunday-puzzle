module AOC201706 where

import Data.Array(Array,array,assocs,bounds)
import Data.Map(Map,empty,insert,member,(!))

import AOC

aoc = AOC {
    day="../../2017/input/06",
    aocTests=[
        AOCTest {
            testData=unlines [
                "0 2 7 0"
                ],
            testResult=Just "5",
            testResult2=Just "4"
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

parse input = array (0,length numbers-1) $ zip [0..] numbers
  where numbers = parseInts input

redistribute :: Array Int Int -> Array Int Int
redistribute blocks = array arrayBounds $ map redist $ assocs blocks
  where
    arrayBounds@(0,n) = bounds blocks
    
    (redistBlocks,minusIndex) = maximum $ map metric $ assocs blocks
    metric (index,nblocks) = (nblocks,-index)

    (d,m) = divMod redistBlocks (n+1)
    redist (index,nblocks)
      | index == -minusIndex = (index,d)
      | (index+minusIndex) `mod` (n+1) <= m = (index,nblocks+d+1)
      | otherwise = (index,nblocks+d)

findCycle :: Ord a => [a] -> (Int,Int)
findCycle = search empty . zip [0..]
  where
    search table ((n,a):rest)
      | member a table = (n,table!a)
      | otherwise = search (insert a n table) rest

result = fst . findCycle . iterate redistribute

result2 = uncurry (-) . findCycle . iterate redistribute
