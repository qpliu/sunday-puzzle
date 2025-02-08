module AOC201509 where

import Data.List(permutations)
import Data.Map(Map,empty,insert,(!))
import qualified Data.Map
import Data.Set(Set,elems,delete,fromList)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2015/input/09",
    aocTests=[
        AOCTest {
            testData=unlines [
                "London to Dublin = 464",
                "London to Belfast = 518",
                "Dublin to Belfast = 141"
                ],
            testResult=Just "605",
            testResult2=Just "982"
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

parse :: String -> (Map (String,String) Int,Set String)
parse = fmap fromList . foldr collect (empty,[]) . map words . lines
  where
    collect [a,"to",b,"=",dist] (dists,locs) =
        (insert (min a b,max a b) (read dist) dists,a:b:locs)

distance :: Map (String,String) Int -> [String] -> Int
distance dists path = sum $ zipWith dist path $ tail path
  where dist a b = dists!(min a b,max a b)

paths :: Set String -> [[String]]
paths = filter f . permutations . elems
  where f list = head list > last list

result ncpu (dists,locs) =
    parallelMapReduce ncpu (distance dists) minimum $ paths locs

result2 ncpu (dists,locs) =
    parallelMapReduce ncpu (distance dists) maximum $ paths locs
