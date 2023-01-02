{-
--- Day 16: Permutation Promenade ---

You come upon a very unusual sight; a group of programs here appear to be
dancing.

There are sixteen programs in total, named a through p. They start by standing
in a line: a stands in position 0, b stands in position 1, and so on until p,
which stands in position 15.

The programs' dance consists of a sequence of dance moves:

 - Spin, written sX, makes X programs move from the end to the front, but
   maintain their order otherwise. (For example, s3 on abcde produces cdeab).
 - Exchange, written xA/B, makes the programs at positions A and B swap places.
 - Partner, written pA/B, makes the programs named A and B swap places.

For example, with only five programs standing in a line (abcde), they could do
the following dance:

 - s1, a spin of size 1: eabcd.
 - x3/4, swapping the last two programs: eabdc.
 - pe/b, swapping programs e and b: baedc.

After finishing their dance, the programs end up in order baedc.

You watch the dance for a while and record their dance moves (your puzzle
input). In what order are the programs standing after their dance?
-}

import Data.Char(isDigit)
import Data.List(sort)

interp :: String -> String -> String
interp ps ('s':x) = front++back
  where
    len = length ps
    n = read x `mod` len
    (back,front) = splitAt (len-n) ps
interp ps ('x':s)
  | i == j = ps
  | otherwise = take i ps ++ ps!!j : drop (i+1) (take j ps) ++ ps!!i : drop (j+1) ps
  where
    (a,s1) = span isDigit s
    b = dropWhile (not . isDigit) s1
    [i,j] = sort [read a,read b]
interp ps ('p':a:'/':b:_) = map swap ps
  where
    swap c | c == a = b | c == b = a | otherwise = c
interp _ insn = error insn

steps :: String -> [String]
steps s | null s = [] | otherwise = step : steps (drop 1 rest)
  where (step,rest) = span (/= ',') s

test :: ()
test
  | interp "abcde" "s1" /= "eabcd" = error "a"
  | interp "eabcd" "x3/4" /= "eabdc" = error "b"
  | interp "eabdc" "pe/b" /= "baedc" = error "c"
  | otherwise = ()

part1 :: IO String
part1 = fmap (foldl interp ['a'..'p'] . steps) $ readFile "input/16.txt"

part2 :: IO String
part2 = do
    s <- fmap steps $ readFile "input/16.txt"
    let f p = foldl interp p s
    let cycle = 1 + length (takeWhile (/= ['a'..'p']) $ tail $ iterate f ['a'..'p'])
    return $ head $ drop (1000000000 `mod` cycle) $ iterate f ['a'..'p']
