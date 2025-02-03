module AOC201712 where

import Data.Map(Map,alter,delete,toList,(!))
import qualified Data.Map
import Data.Set(Set,elems,empty,fromList,insert,member,singleton,size)

import AOC

aoc = AOC {
    day="../../2017/input/12",
    aocTests=[
        AOCTest {
            testData=unlines [
                "0 <-> 2",
                "1 <-> 1",
                "2 <-> 0, 3, 4",
                "3 <-> 2, 4",
                "4 <-> 2, 3, 6",
                "5 <-> 6",
                "6 <-> 4, 5"
                ],
            testResult=Just "6",
            testResult2=Just "2"
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

parse = foldr collect Data.Map.empty . map words . lines
  where
    collect (a:"<->":bs) graph =
        foldr (link a) graph $ map (filter (/= ',')) bs
    link a b = alter (Just . maybe (singleton b) (insert b)) a
             . alter (Just . maybe (singleton a) (insert a)) b

walk :: [String] -> Set String -> Map String (Set String) -> Set String
walk [] set _ = set
walk (p:queue) set graph
  | member p set = walk queue set graph
  | otherwise = walk (elems (graph!p) ++ queue) (insert p set) graph

result = size . walk ["0"] empty

groups :: Map String (Set String) -> Int
groups graph
  | null graph = 0
  | otherwise = 1 + groups (foldr delete graph (elems group))
  where
    ((start,_):_) = toList graph
    group = walk [start] empty graph

result2 = groups
