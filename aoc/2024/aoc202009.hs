module AOC202009 where

import Data.Set(Set,delete,elems,empty,fromList,insert,member)

import AOC

aoc = AOC {
    day="../../2020/input/09",
    aocTests=[
        AOCTest {
            testData=unlines [
                "35",
                "20",
                "15",
                "25",
                "47",
                "40",
                "62",
                "55",
                "65",
                "95",
                "102",
                "117",
                "150",
                "182",
                "127",
                "219",
                "299",
                "277",
                "309",
                "576"
                ],
            testResult=Just "127",
            testResult2=Just "62"
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=parseInts,
        codeTest=result 5,
        codeTest2=result2 5,
        codeResult=result 25,
        codeResult2=result2 25
        }
    }

search :: Set Int -> [Int] -> [Int] -> Int
search window (removal:removals) (add:adds)
  | null [() | n <- elems window, member (add-n) window] = add
  | otherwise = search (insert add (delete removal window)) removals adds

result n numbers = search (fromList $ take n numbers) numbers $ drop n numbers

search2 :: Int -> Int -> Set Int -> [Int] -> [Int] -> Int
search2 target windowSum window removals@(r:rs) adds@(a:as)
  | windowSum < target =
      search2 target (windowSum+a) (insert a window) removals as
  | windowSum > target =
      search2 target (windowSum-r) (delete r window) rs adds
  | otherwise = minimum window + maximum window

result2 n numbers = search2 (result n numbers) 0 empty numbers numbers
