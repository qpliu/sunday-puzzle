module AOC201804 where

import Data.List(sort)
import Data.Map(Map,alter,empty,findWithDefault,insert,toList)
import Data.Tuple(swap)

import AOC

aoc = AOC {
    day="../../2018/input/04",
    aocTests=[
        AOCTest {
            testData = unlines [
                "[1518-11-01 00:00] Guard #10 begins shift",
                "[1518-11-01 00:05] falls asleep",
                "[1518-11-01 00:25] wakes up",
                "[1518-11-01 00:30] falls asleep",
                "[1518-11-01 00:55] wakes up",
                "[1518-11-01 23:58] Guard #99 begins shift",
                "[1518-11-02 00:40] falls asleep",
                "[1518-11-02 00:50] wakes up",
                "[1518-11-03 00:05] Guard #10 begins shift",
                "[1518-11-03 00:24] falls asleep",
                "[1518-11-03 00:29] wakes up",
                "[1518-11-04 00:02] Guard #99 begins shift",
                "[1518-11-04 00:36] falls asleep",
                "[1518-11-04 00:46] wakes up",
                "[1518-11-05 00:03] Guard #99 begins shift",
                "[1518-11-05 00:45] falls asleep",
                "[1518-11-05 00:55] wakes up"
                ],
            testResult=Just "240",
            testResult2=Just "4455"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result metric1,
        codeTest2=result metric2,
        codeResult=result metric1,
        codeResult2=result metric2
        }
    }

parse = sort . map (map abs . parseInts) . lines

collect :: Map Int (Map Int Int) -> [[Int]] -> Map Int (Map Int Int)
collect table [] = table
collect table ([_,_,_,_,_,guardID]:rest) = collectGuard table guardID rest
collect table rows = error (show (length rows,take 5 rows))

collectGuard :: Map Int (Map Int Int) -> Int -> [[Int]]
             -> Map Int (Map Int Int)
collectGuard table guardID ([_,_,_,_,start]:[_,_,_,_,end]:rest) =
    collectGuard (insert guardID (foldr incr byMinute [start..end-1]) table)
                 guardID rest
  where
    byMinute = findWithDefault empty guardID table
    incr = alter (Just . maybe 1 succ)
collectGuard table guardID rest = collect table rest

metric1 :: (Int,Map Int Int) -> (Int,Int)
metric1 (guardID,byMinute) =
    (sum byMinute,guardID*(snd $ maximum $ map swap $ toList byMinute))

result metric = snd . maximum . map metric . toList . collect empty

metric2 :: (Int,Map Int Int) -> (Int,Int)
metric2 (guardID,byMinute) =
    (maximum byMinute,guardID*(snd $ maximum $ map swap $ toList byMinute))
