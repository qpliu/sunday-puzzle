module AOC202403 where

import Data.Char(isDigit)

import AOC

aoc = AOC {
    day="03",
    testData="xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))",
    testResult="161",
    testData2="xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))",
    testResult2="48",
    aocParse=parse,
    aocTest=sum,
    aocResult=sum,
    aocParse2=parse2,
    aocTest2=sum,
    aocResult2=sum
    }

parse :: String -> [Int]
parse [] = []
parse ('m':'u':'l':'(':cs) = mul1 cs
  where
    mul1 str
      | length a `elem` [1..3] && take 1 cs == "," = mul2 (read a) (tail cs)
      | otherwise = parse str
      where (a,cs) = span isDigit str
    mul2 a str
      | length b `elem` [1..3] && take 1 cs == ")" = (a*read b) : parse cs
      | otherwise = parse str
      where (b,cs) = span isDigit str
parse (_:cs) = parse cs

parse2 :: String -> [Int]
parse2 [] = []
parse2 ('m':'u':'l':'(':cs) = mul1 cs
  where
    mul1 str
      | length a `elem` [1..3] && take 1 cs == "," = mul2 (read a) (tail cs)
      | otherwise = parse2 str
      where (a,cs) = span isDigit str
    mul2 a str
      | length b `elem` [1..3] && take 1 cs == ")" = (a*read b) : parse2 cs
      | otherwise = parse2 str
      where (b,cs) = span isDigit str
parse2 ('d':'o':'n':'\'':'t':'(':')':cs) = dont cs
  where
    dont [] = []
    dont ('d':'o':'(':')':cs) = parse2 cs
    dont (_:cs) = dont cs
parse2 (_:cs) = parse2 cs

{-
data Parsed = Corrupt String | Do | Dont | Mul Int Int deriving Show

parse = p ""
  where
    p prev "" = corrupt prev []
    p prev ('d':'o':'(':')':cs) = corrupt prev $ Do : p "" cs
    p prev ('d':'o':'n':'\'':'t':'(':')':cs) = corrupt prev $ Dont : p "" cs
    p prev ('m':cs) = maybe (p ('m':prev) cs) id (mul prev cs)
    p prev (c:cs) = p (c:prev) cs
    corrupt "" rest = rest
    corrupt prev rest = Corrupt (reverse prev) : rest
    mul prev ('u':'l':'(':cs) = mul1 prev cs
    mul _ _ = Nothing
    mul1 prev str
      | length a `elem` [1..3] && take 1 cs == "," = mul2 prev a (tail cs)
      | otherwise = Nothing
      where (a,cs) = span isDigit str
    mul2 prev a str
      | length b `elem` [1..3] && take 1 cs == ")" =
          Just $ corrupt prev $ (Mul (read a) (read b)):p "" (tail cs)
      | otherwise = Nothing
      where (b,cs) = span isDigit str

result = sum . map mul
  where
    mul (Mul a b) = a*b
    mul _ = 0

result2 = fst . foldl execute (0,True)
  where
    execute (total,_) Do = (total,True)
    execute (total,_) Dont = (total,False)
    execute (total,True) (Mul a b) = (total+a*b,True)
    execute x _ = x
-}
