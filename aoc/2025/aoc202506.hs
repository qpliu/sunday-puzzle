module AOC202506 where

import AOC

aoc = AOC {
    day="06",
    aocTests=[
        AOCTest {
            testData=unlines [
                "123 328  51 64 ",
                " 45 64  387 23 ",
                "  6 98  215 314",
                "*   +   *   +  "
            ],
            testResult=Just "4277556",
            testResult2=Just "3263827"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=sum,
        codeResult=result,
        codeResult2=sum
        }
    }

parse = p . reverse . lines
  where
    p (line:rest) = (map parseOp (words line),map (map read . words) rest)
    parseOp "+" = (0,(+))
    parseOp "*" = (1,(*))

result ([],_) = 0
result ((z,op):ops,rows) = foldr op z (map head rows) + result (ops,map tail rows)

parse2 = parseCols 0 (+) . lines

parseCols n op ("":_) = [n]
parseCols n op text
  | null items = n : parseCols 0 (+) (map tail text)
  | last col == '+' = parseCols (foldr (+) 0 items) (+) (map tail text)
  | last col == '*' = parseCols (foldr (*) 1 items) (*) (map tail text)
  | otherwise = parseCols (foldr op n items) op (map tail text)
  where
    col = map head text
    items = map read $ words $ init col
