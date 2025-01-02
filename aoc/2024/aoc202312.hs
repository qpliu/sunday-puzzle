module AOC202312 where

import Data.List(intercalate)

import AOC

aoc = AOC {
    day="../../2023/input/12",
    aocTests=[
        AOCTest {
            testData=unlines [
                "???.### 1,1,3",
                ".??..??...?##. 1,1,3",
                "?#?#?#?#?#?#?#? 1,3,1,6",
                "????.#...#... 4,1,1",
                "????.######..#####. 1,6,5",
                "?###???????? 3,2,1"
                ],
            testResult=Just "21",
            testResult2=Just "525152"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse2,
        pcodeTest=result,
        pcodeTest2=result,
        pcodeResult=result,
        pcodeResult2=result
        }
    }

parse :: String -> [(String,[Int])]
parse = map p . lines
  where p line = (takeWhile (/= ' ') line,parseInts line)

result :: Int -> [(String,[Int])] -> Int
result ncpu = parallelMapReduce ncpu (evalMemoized . arrangements) sum

arrangements :: (String,[Int]) -> Memo (String,[Int]) Int Int
arrangements = memoize arr
  where
    arr (str,[])
      | any (== '#') str = return 0
      | otherwise = return 1
    arr (str,(n:rest))
      | length str < n = return 0
      | any (== '.') (take n str) && any (== '#') (takeWhile (/= '.') str) =
          return 0
      | any (== '.') (take n str) =
          arrangements (dropWhile (== '.') $ dropWhile (/= '.') str,n:rest)
      | "#" == take 1 str && "#" == take 1 (drop n str) = return 0
      | "#" == take 1 str = arrangements (drop (n+1) str,rest)
      | "#" == take 1 (drop n str) = arrangements (drop 1 str,n:rest)
      | otherwise = do
          count1 <- arrangements (drop (n+1) str,rest)
          count2 <- arrangements (drop 1 str,n:rest)
          return $ count1+count2

parse2 :: String -> [(String,[Int])]
parse2 = map unfold . parse
  where unfold (str,ns) =
            (intercalate "?" $ replicate 5 str,concat $ replicate 5 ns)
