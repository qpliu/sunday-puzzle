module AOC202403 where

import Data.Char(isDigit)

import AOC

aoc = AOC {
    day="03",
    aocTests=[
        AOCTest {
            testData="xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))",
            testResult=Just "161",
            testResult2=Nothing
            },
        AOCTest {
            testData="xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))",
            testResult=Nothing,
            testResult2=Just "48"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=sum,
        codeTest2=sum,
        codeResult=sum,
        codeResult2=sum
        }
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
