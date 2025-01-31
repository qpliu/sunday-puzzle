module AOC201807 where

import Data.Char(ord)
import Data.List(partition)
import Data.Map(Map,alter,toList)
import qualified Data.Map
import Data.Set(Set,delete,difference,fromList,insert,singleton)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2018/input/07",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Step C must be finished before step A can begin.",
                "Step C must be finished before step F can begin.",
                "Step A must be finished before step B can begin.",
                "Step A must be finished before step D can begin.",
                "Step B must be finished before step E can begin.",
                "Step D must be finished before step E can begin.",
                "Step F must be finished before step E can begin."
                ],
            testResult=Just $ show "CABDFE",
            testResult2=Just "15"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2 0 2,
        codeResult=result,
        codeResult2=result2 60 5
        }
    }

parse = toList . foldr collect Data.Map.empty . map words . lines
  where
    collect ["Step",[prereq],"must","be","finished","before",
             "step",[step],"can","begin."] =
        alter (Just . maybe (singleton prereq) (insert prereq)) step .
        alter (Just . maybe Data.Set.empty id) prereq

process :: [(Char,Set Char)] -> [(Char,Set Char)] -> String
process [] [] = []
process skipped ((step,prereqs):rest)
  | null prereqs =
      step : process [] (map (fmap (delete step)) (reverse skipped ++ rest))
  | otherwise = process ((step,prereqs):skipped) rest

result = process []

process2 :: Int -> [(Char,Set Char)] -> [(Char,Int)]
         -> Int -> Int -> [(Char,Set Char)] -> Int
process2 t [] workers baseTime nworkers [] = t + maximum (0:map snd workers)
process2 t skipped workers baseTime nworkers tasks
  | nworkers == 0 || null tasks =
      process2 (t+dt) [] (map (fmap (+(-dt))) working) baseTime
               (nworkers+length idle) (map (fmap (`difference` completed))
                                           (reverse skipped++tasks))
  | null prereqs = 
      process2 t skipped ((step,taskTime):workers) baseTime (nworkers-1) rest
  | otherwise =
      process2 t ((step,prereqs):skipped) workers baseTime nworkers rest
  where
    dt = minimum $ map snd workers
    (idle,working) = partition ((== dt) . snd) workers
    completed = fromList $ map fst idle

    ((step,prereqs):rest) = tasks
    taskTime = baseTime + 1 + ord step - ord 'A'

result2 = process2 0 [] []
