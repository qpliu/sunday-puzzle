module AOC202018 where

import Data.Char(isDigit)

import AOC

aoc = AOC {
    day="../../2020/input/18",
    aocTests=[
        AOCTest {
            testData="1 + 2 * 3 + 4 * 5 + 6",
            testResult=Just "71",
            testResult2=Just "231"
            },
        AOCTest {
            testData="1 + (2 * 3) + (4 * (5 + 6))",
            testResult=Just "51",
            testResult2=Just "51"
            },
        AOCTest {
            testData="2 * 3 + (4 * 5)",
            testResult=Just "26",
            testResult2=Just "46"
            },
        AOCTest {
            testData="5 + (8 * 3 + 9 + 3 * 4 * 3)",
            testResult=Just "437",
            testResult2=Just "1445"
            },
        AOCTest {
            testData="5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
            testResult=Just "12240",
            testResult2=Just "669060"
            },
        AOCTest {
            testData="((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 ",
            testResult=Just "13632",
            testResult2=Just "23340"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

parse = map p . lines
  where
    p [] = []
    p (c:cs)
      | isDigit c = Left (read (c:n) :: Int) : p rest
      | elem c "()*+" = Right c : p cs
      | otherwise = p cs
      where (n,rest) = span isDigit cs

eval :: [Either Int (Int -> Int)] -> [Either Int Char] -> Int
eval stack (Right '(':expr) = eval (Right id:stack) expr
eval [] (Left n:expr) = eval [Left n] expr
eval (Left n:stack) (Right '*':expr) = eval (Right (n*):stack) expr
eval (Left n:stack) (Right '+':expr) = eval (Right (n+):stack) expr
eval (Right f:stack) (Left n:expr) = eval (Left (f n):stack) expr
eval (Left n:Right f:stack) (Right ')':expr) = eval (Left (f n):stack) expr
eval [Left n] (Right ')':expr) = eval [Left n] expr
eval [Left n] [] = n

result ncpu = parallelMapReduce ncpu (eval []) sum

data Stack = Num Int | Sum Int | Prod Int | Open deriving Show

eval2 :: [Stack] -> [Either Int Char] -> Int
eval2 stack (Right '(':expr) = eval2 (Open:stack) expr
eval2 [] (Left m:expr) = eval2 [Num m] expr
eval2 stack@(Open:_) (Left m:expr) = eval2 (Num m:stack) expr
eval2 (Num n:stack) (Right '*':expr) = eval2 (Prod n:stack) expr
eval2 (Num n:stack) (Right '+':expr) = eval2 (Sum n:stack) expr
eval2 (Sum n:stack) (Left m:expr) = eval2 (Num (n+m):stack) expr
eval2 stack@(Prod _:_) (Left m:expr) = eval2 (Num m:stack) expr
eval2 (Num n:Open:Sum m:stack) (Right ')':expr) = eval2 (Num (n+m):stack) expr
eval2 (Num n:Open:stack) (Right ')':expr) = eval2 (Num n:stack) expr
eval2 (Num n:Prod m:stack) expr@(Right ')':_) = eval2 (Num (n*m):stack) expr
eval2 [Num n] [] = n
eval2 (Num n:Prod m:stack) [] = eval2 (Num (n*m):stack) []
eval2 stack expr = error (show (stack,expr))

result2 ncpu = parallelMapReduce ncpu (eval2 []) sum
