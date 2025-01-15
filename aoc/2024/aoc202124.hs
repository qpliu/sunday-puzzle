module AOC202124 where

import Data.List(sort)

import AOC

aoc = AOC {
    day="../../2021/input/24",
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

{-
inp w
mul x 0
add x z
mod x 26
div z A  -- A is 1 or 26
add x B
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y C
mul y x
add z y

if input = (z mod 26) + B then
  z = z div A
else
  z = (z div A)*26 + C + input
end if

If A = 1, the if condition is always false, since B >= 10 whenever A = 1,
and input_1+C is pushed onto z.

If A = 26, the if condition must be made true.
input_1+C is popped off of z, and the condition is input_1+C = input-B,
determining input_1 and input.

-}

parse = zip [1..] . p . words
  where
    p [] = []
    p ("inp":"w":"mul":"x":"0":"add":"x":"z":"mod":"x":"26":"div":"z":a
       :"add":"x":b
       :"eql":"x":"w":"eql":"x":"0":"mul":"y":"0":"add":"y":"25":"mul":"y":"x"
       :"add":"y":"1":"mul":"z":"y":"mul":"y":"0":"add":"y":"w":"add":"y":c
       :"mul":"y":"x":"add":"z":"y":rest) =
            (read a :: Int,read b :: Int,read c :: Int) : p rest


fromDigits :: [(Int,Int)] -> Int
fromDigits = foldl collect 0 . sort
  where collect a (_,b) = a*10+b

find :: (Int -> (Int,Int)) -> [(Int,(Int,Int,Int))] -> Int
find chooseInputs = fromDigits . snd . foldl collect ([],[])
  where
    collect (z,done) (digit,(1,b,c)) = ((digit,c):z,done)
    collect ((digit1,c):z,done) (digit,(26,b,_)) =
        (z,(digit,input):(digit1,input1):done)
      where (input1,input) = chooseInputs (b+c)

result = find chooseInputs
  where chooseInputs bc | bc >= 0 = (9-bc,9)
                        | otherwise = (9,9+bc)

result2 = find chooseInputs
  where chooseInputs bc | bc >= 0 = (1,1+bc)
                        | otherwise = (1-bc,1)
