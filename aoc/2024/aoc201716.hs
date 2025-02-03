module AOC201716 where

import Data.Char(isDigit)
import Data.List(sort)
import Data.Map(Map,empty,fromList,insert,member,toList,(!))
import Data.Tuple(swap)

import AOC

aoc = AOC {
    day="../../2017/input/16",
    aocTests=[
        AOCTest {
            testData="s1,x3/4,pe/b",
            testResult=Just $ show "baedc",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 5,
        codeTest2=result2 5 2,
        codeResult=result 16,
        codeResult2=result2 16 1000000000
        }
    }

data Move = S Int | X Int Int | P Char Char deriving Show

parse (',':rest) = parse rest
parse ('s':rest) = S (read n) : parse rest2
  where (n,rest2) = span isDigit rest
parse ('x':rest) = X (read n1) (read n2) : parse rest2
  where
    (n1,'/':rest1) = span isDigit rest
    (n2,rest2) = span isDigit rest1
parse ('p':a:'/':b:rest) = P a b : parse rest
parse "" = []
parse "\n" = []

dance :: Int -> [(Int,Char)] -> Move -> [(Int,Char)]
dance nprograms programs (S n) = map s programs
  where
    pivot = nprograms-n
    s (i,c)
      | i < pivot = (i+n,c)
      | otherwise = (i-pivot,c)
dance nprograms programs (X j k) = map x programs
  where
    x (i,c)
      | i == j = (k,c)
      | i == k = (j,c)
      | otherwise = (i,c)
dance nprograms programs (P a b) = map p programs
  where
    [(ia,_)] = filter ((== a) . snd) programs
    [(ib,_)] = filter ((== b) . snd) programs
    p (i,c)
      | c == a = (ib,c)
      | c == b = (ia,c)
      | otherwise = (i,c)

result nprograms =
    map snd . sort . foldl (dance nprograms) (zip [0..nprograms-1] ['a'..])

findCycle :: Ord a => [a] -> Int -> a
findCycle = search empty . zip [0..]
  where
    search table ((i,v):rest)
      | member v table =
          lookup i (table!v) (fromList $ map swap $ toList table)
      | otherwise = search (insert v i table) rest
    lookup iNextRecur iRecur table i
      | i < iNextRecur = table!i
      | otherwise = table!(iRecur + (i-iRecur) `mod` (iNextRecur-iRecur))

result2 nprograms niterations steps = cycle niterations
  where
    cycle = findCycle $ map (map snd . sort)
                      $ iterate step (zip [0..nprograms-1] ['a'..])
    step programs = foldl (dance nprograms) programs steps
