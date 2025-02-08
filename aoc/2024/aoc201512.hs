module AOC201512 where

import Data.Char(isDigit)

import AOC

aoc = AOC {
    day="../../2015/input/12",
    aocTests=[
        AOCTest {
            testData="[1,2,3]",
            testResult=Just "6",
            testResult2=Just "6"
            },
        AOCTest {
            testData="{\"a\":2,\"b\":4}",
            testResult=Just "6",
            testResult2=Nothing
            },
        AOCTest {
            testData="[[[3]]]",
            testResult=Just "3",
            testResult2=Nothing
            },
        AOCTest {
            testData="{\"a\":{\"b\":4},\"c\":-1}",
            testResult=Just "3",
            testResult2=Nothing
            },
        AOCTest {
            testData="{\"a\":[-1,1]}",
            testResult=Just "0",
            testResult2=Nothing
            },
        AOCTest {
            testData="[-1,{\"a\":1}]",
            testResult=Just "0",
            testResult2=Nothing
            },
        AOCTest {
            testData="[]",
            testResult=Just "0",
            testResult2=Nothing
            },
        AOCTest {
            testData="{}",
            testResult=Just "0",
            testResult2=Nothing
            },
        AOCTest {
            testData="[1,{\"c\":\"red\",\"b\":2},3]",
            testResult=Nothing,
            testResult2=Just "4"
            },
        AOCTest {
            testData="{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}",
            testResult=Nothing,
            testResult2=Just "0"
            },
        AOCTest {
            testData="[1,\"red\",5]",
            testResult=Nothing,
            testResult2=Just "6"
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=parse,
        codeTest=sum,
        codeTest2=result,
        codeResult=sum,
        codeResult2=result
        }
    }

tokenize :: String -> [String]
tokenize (c:cs)
  | c == '"' =
      let (str,'"':rest) = span (/= '"') cs
      in  if str == "red"
            then "r":tokenize rest
            else "s":tokenize rest
  | c == '-' || isDigit c =
      let (n,rest) = span isDigit cs in (c:n):tokenize rest
  | elem c "{}:[" = [c]:tokenize cs
  | otherwise = tokenize cs
tokenize [] = []

parse = tokenize

collect :: [(Maybe [String])] -> [String] -> [String]
collect stack@(Nothing:_) ("{":rest) = collect (Nothing:stack) rest
collect (Nothing:stack) ("}":rest) = collect stack rest
collect stack@(Nothing:_) (_:rest) = collect stack rest
collect stack ("{":rest) = collect (Just []:stack) rest
collect (Just _:stack) ("s":":":"r":rest) = collect (Nothing:stack) rest
collect (Just i:Just j:stack) ("}":rest) = collect (Just (i++j):stack) rest
collect stack@(Just i:stack2) (s:rest)
  | elem s [":","s","r","["] = collect stack rest
  | otherwise = collect (Just (s:i):stack2) rest
collect (Nothing:_) [] = []
collect (Just i:_) [] = i

result = sum . concatMap parseInts . collect [Just []]
