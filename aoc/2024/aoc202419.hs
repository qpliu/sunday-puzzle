module AOC202419 where

import Data.Set(Set,fromList,member)

import AOC

aoc = AOC {
    day="19",
    aocTests=[
        AOCTest {
            testData=unlines [
                "r, wr, b, g, bwu, rb, gb, br",
                "",
                "brwrr",
                "bggr",
                "gbbr",
                "rrbgbr",
                "ubwu",
                "bwurrg",
                "brgr",
                "bbrgwb"
                ],
            testResult=Just "6",
            testResult2=Just "16"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=const result,
        pcodeTest2=result2,
        pcodeResult=const result,
        pcodeResult2=result2
        }
    }

maxLen = 8 -- hard coded maximum pattern length

parse :: String -> (Set String,[String])
parse = p . lines
  where p (patterns:"":designs) =
            (fromList $ words $ filter (/= ',') patterns,designs)

possible :: Set String -> String -> Bool
possible patterns design = evalMemoized (possibleM design)
  where
    possibleM = memoize p
    p design
      | null design = return True
      | otherwise = check [1..maxLen]
      where
        check [] = return False
        check (n:ns)
          | take n design `member` patterns = do
              res <- possibleM (drop n design)
              if res
                then return True
                else check ns
          | otherwise = check ns

result (patterns,designs) = length $ filter (possible patterns) designs

ways :: Set String -> String -> Int
ways patterns design = evalMemoized (waysM design)
  where
    waysM = memoize w
    w design
      | null design = return 1
      | otherwise = do
          results <- mapM check [1..min maxLen (length design)]
          return $ sum results
      where
        check n
          | take n design `member` patterns = waysM $ drop n design
          | otherwise = return 0

result2 ncpu (patterns,designs) =
    parallelMapReduce ncpu (ways patterns) sum designs
