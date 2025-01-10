module AOC202221 where

import Data.Map(Map,fromList,(!))

import AOC

aoc = AOC {
    day="../../2022/input/21",
    aocTests=[
        AOCTest {
            testData=unlines [
                "root: pppw + sjmn",
                "dbpl: 5",
                "cczh: sllz + lgvd",
                "zczc: 2",
                "ptdq: humn - dvpt",
                "dvpt: 3",
                "lfqf: 4",
                "humn: 5",
                "ljgn: 2",
                "sjmn: drzm * dbpl",
                "sllz: 4",
                "pppw: cczh / lfqf",
                "lgvd: ljgn * ptdq",
                "drzm: hmdt - zczc",
                "hmdt: 32"
                ],
            testResult=Just "152",
            testResult2=Just "301"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse :: String -> Map String Int
parse input = table
  where
    table = fromList $ map (p . words) $ lines input
    p [monkey,value] = (init monkey,read value)
    p [monkey,lhs,"+",rhs] = (init monkey,table!lhs + table!rhs)
    p [monkey,lhs,"-",rhs] = (init monkey,table!lhs - table!rhs)
    p [monkey,lhs,"*",rhs] = (init monkey,table!lhs * table!rhs)
    p [monkey,lhs,"/",rhs] = (init monkey,table!lhs `div` table!rhs)

data Expr = H | N Int           | Root  Expr Expr
              | Plus  Expr Expr | Minus Expr Expr
              | Times Expr Expr | Div   Expr Expr
    deriving Show

parse2 :: String -> Map String Expr
parse2 input = table
  where
    table = fromList $ map (p . words) $ lines input
    p ["humn:",_] = ("humn",H)
    p ["root:",lhs,_,rhs] = ("root",ex Root undefined (table!lhs) (table!rhs))
    p [monkey,value] = (init monkey,N (read value))
    p [monkey,lhs,"+",rhs] = (init monkey,ex Plus  (+) (table!lhs) (table!rhs))
    p [monkey,lhs,"-",rhs] = (init monkey,ex Minus (-) (table!lhs) (table!rhs))
    p [monkey,lhs,"*",rhs] = (init monkey,ex Times (*) (table!lhs) (table!rhs))
    p [monkey,lhs,"/",rhs] = (init monkey,ex Div   div (table!lhs) (table!rhs))

    ex cons op (N lhs) (N rhs) = (N (op lhs rhs))
    ex cons op lhs rhs = cons lhs rhs

result = (!"root")

invert :: Int -> Expr -> Int
invert a H = a
invert a (Plus (N b) expr) = invert (a-b) expr
invert a (Plus expr (N b)) = invert (a-b) expr
invert a (Minus (N b) expr) = invert (b-a) expr
invert a (Minus expr (N b)) = invert (a+b) expr
invert a (Times (N b) expr) = invert (a `div` b) expr
invert a (Times expr (N b)) = invert (a `div` b) expr
invert a (Div (N b) expr) = invert (b `div` a) expr
invert a (Div expr (N b)) = invert (a*b) expr

result2 = solve . (!"root")
  where
    solve (Root (N a) expr) = invert a expr
    solve (Root expr (N a)) = invert a expr
