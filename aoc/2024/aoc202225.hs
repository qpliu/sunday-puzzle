module AOC202225 where

import AOC

aoc = AOC {
    day="../../2022/input/25",
    aocTests=[
        AOCTest {
            testData=unlines [
                "1=-0-2",
                "12111",
                "2=0=",
                "21",
                "2=01",
                "111",
                "20012",
                "112",
                "1=-1=",
                "1-12",
                "12",
                "1=",
                "122"
                ],
            testResult=Just (show "2=-1=0"),
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse = map reverse . lines

add :: Int -> String -> String -> String
add c [] []
  | c == 1 = "1"
  | c == 0 = ""
  | c == -1 = "-"
add c [] (b:bs) = digit : add carry [] bs
  where (carry,digit) = addColumn (c+snafu b)
add c (a:as) [] = digit : add carry [] as
  where (carry,digit) = addColumn (c+snafu a)
add c (a:as) (b:bs) = digit : add carry as bs
  where (carry,digit) = addColumn (c+snafu a+snafu b)

snafu :: Char -> Int
snafu '0' = 0
snafu '1' = 1
snafu '2' = 2
snafu '-' = -1
snafu '=' = -2

addColumn :: Int -> (Int,Char)
addColumn x
  | x == -5 = (-1,'0')
  | x == -4 = (-1,'1')
  | x == -3 = (-1,'2')
  | x == -2 = (0,'=')
  | x == -1 = (0,'-')
  | x ==  0 = (0,'0')
  | x ==  1 = (0,'1')
  | x ==  2 = (0,'2')
  | x ==  3 = (1,'=')
  | x ==  4 = (1,'-')
  | x ==  5 = (1,'0')

result = reverse . foldr (add 0) "0"

result2 = const ()
