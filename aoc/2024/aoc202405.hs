module AOC202405 where

import Data.List(groupBy,sortBy)
import Data.Set(Set,fromList,member)

import AOC

aoc = AOC {
    day="05",
    testData=unlines [
    "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13",
    "",
    "75,47,61,53,29",
    "97,61,53,29,13",
    "75,29,13",
    "75,97,47,61,53",
    "61,13,29",
    "97,13,75,29,47"
    ],
    testResult="143",
    testData2="",
    testResult2="123",
    aocParse=parse,
    aocResult=result,
    aocParse2=parse,
    aocResult2=result2
    }

parse :: String -> (Set [Int],[[Int]])
parse = p . span (not . null) . lines
  where
    p (rules,updates) = (fromList $ map pRule rules,map pUpdate (tail updates))
    pRule = map read . filter (/= "|") . groupBy (\ a b -> a /= '|' && b /= '|')
    pUpdate = map read . filter (/= ",") . groupBy (\ a b -> a /= ',' && b /= ',')

ordered rules pages =
    and [not ([b,a] `member` rules) | (a,i) <- zip pages [1..],
                                      (b,j) <- zip pages [1..], i < j]

result (rules,updates) = sum $ map middle updates
  where
    middle pages
      | ordered rules pages = head $ drop (length pages `div` 2) pages
      | otherwise = 0

result2 (rules,updates) = sum $ map middle updates
  where
    middle pages
      | ordered rules pages = 0
      | otherwise = head $ drop (length pages `div` 2) $ sortBy cmp pages
    cmp a b
      | [a,b] `member` rules = LT
      | [b,a] `member` rules = GT
      | otherwise = EQ
