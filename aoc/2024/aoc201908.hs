module AOC201908 where

import Data.Char(isSpace)

import AOC

aoc = AOC {
    day="../../2019/input/08",
    aocTests=[
        AOCTest {
            testData="123456789012",
            testResult=Just "1",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result 3 2,
        codeTest2=result2,
        codeResult=result 25 6,
        codeResult2=result2
        }
    }

parse = filter (not . isSpace)

group :: Int -> [a] -> [[a]]
group n = g . splitAt n
  where
    g ([],[]) = []
    g (as,rest) = as : group n rest

result w h = snd . minimum . map scan . group (w*h)
  where scan image = (count '0',count '1'*count '2')
          where count c = length $ filter (== c) image

parse2 = group (25*6) . filter (not . isSpace)

pixel :: Char -> Char -> Char
pixel '0' _ = '.'
pixel '1' _ = '#'
pixel '2' p = p

result2 = ocr4x6 . unlines . group 25 . foldr (zipWith pixel) (repeat '.')
