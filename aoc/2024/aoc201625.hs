module AOC201625 where

import AOC

aoc = AOC {
    day="../../2016/input/25",
    aocTests=[],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=const (),
        codeResult=result,
        codeResult2=const ()
        }
    }

{-
Decompiling:
d = a+parameter1*parameter2
loop
  a = d
  loop
    out low bit of a
    a = a div 2
    if a = 0 break
  end loop
end loop
-}

parse = take 2 . parseInts

search :: Int -> Int -> Int
search d p
  | d < p = search (d*4+2) p
  | otherwise = d - p

result = search 2 . product
