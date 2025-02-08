module AOC201516 where

import AOC

aoc = AOC {
    day="../../2015/input/16",
    aocTests=[],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parseAunt :: String -> (Int,[(Int,String)])
parseAunt s = (aunt,zip counts $ items $ drop 2 $ words s)
  where
    (aunt:counts) = parseInts s
    items (a:_:rest) = a:items rest
    items [] = []

parse = map parseAunt . lines

matches :: ((Int,String) -> Bool) -> (Int,[(Int,String)]) -> Bool
matches valid (_,items) = all valid items

valid1 :: (Int,String) -> Bool
valid1 (n,"children:") = n == 3
valid1 (n,"cats:") = n == 7
valid1 (n,"samoyeds:") = n == 2
valid1 (n,"pomeranians:") = n == 3
valid1 (n,"akitas:") = n == 0
valid1 (n,"vizslas:") = n == 0
valid1 (n,"goldfish:") = n == 5
valid1 (n,"trees:") = n == 3
valid1 (n,"cars:") = n == 2
valid1 (n,"perfumes:") = n == 1

result = fst . head . filter (matches valid1)

valid2 :: (Int,String) -> Bool
valid2 (n,"children:") = n == 3
valid2 (n,"cats:") = n > 7
valid2 (n,"samoyeds:") = n == 2
valid2 (n,"pomeranians:") = n < 3
valid2 (n,"akitas:") = n == 0
valid2 (n,"vizslas:") = n == 0
valid2 (n,"goldfish:") = n < 5
valid2 (n,"trees:") = n > 3
valid2 (n,"cars:") = n == 2
valid2 (n,"perfumes:") = n == 1

result2 = fst . head . filter (matches valid2)
