module AOC202010 where

import Data.List(sort)

import AOC

aoc = AOC {
    day="../../2020/input/10",
    aocTests=[
        AOCTest {
            testData=unlines [
                "16",
                "10",
                "15",
                "5",
                "1",
                "11",
                "7",
                "19",
                "6",
                "12",
                "4"
                ],
            testResult=Just "35",
            testResult2=Just "8"
            },
        AOCTest {
            testData=unlines [
                "28",
                "33",
                "18",
                "42",
                "31",
                "14",
                "46",
                "20",
                "48",
                "47",
                "24",
                "23",
                "49",
                "45",
                "19",
                "38",
                "39",
                "11",
                "1",
                "32",
                "25",
                "35",
                "8",
                "17",
                "7",
                "9",
                "4",
                "2",
                "34",
                "10",
                "3"
                ],
            testResult=Just "220",
            testResult2=Just "19208"
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

parse = sort . parseInts

result adapters =
    (1 + length (filter (== 3) diffs)) * length (filter (== 1) diffs)
  where
    diffs = zipWith (-) adapters (0:adapters)

splitAt3Diff :: [Int] -> [[Int]]
splitAt3Diff numbers =
    split $ zip numbers $ zipWith (-) (tail numbers) numbers ++ [3]
  where
    split [] = []
    split nums = map fst (seg ++ take 1 rest) : split (drop 1 rest)
      where (seg,rest) = span ((< 3) . snd) nums

countWays :: [Int] -> Int
countWays [] = 1
countWays [_] = 1
countWays [_,_] = 1
countWays [n1,n2,n3] | n1+3 >= n3 = 2 | otherwise = 1
countWays (n1:rest2@(n2:rest3@(n3:rest4@(n4:_))))
  | n1+3 >= n4 = countWays rest2 + countWays rest3 + countWays rest4
  | n1+3 >= n3 = countWays rest2 + countWays rest3
  | otherwise = countWays rest2

result2 = product . map countWays . splitAt3Diff . (0:)
