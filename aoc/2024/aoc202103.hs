module AOC202103 where

import AOC

aoc = AOC {
    day="../../2021/input/03",
    aocTests=[
        AOCTest {
            testData=unlines [
                "00100",
                "11110",
                "10110",
                "10111",
                "10101",
                "01111",
                "00111",
                "11100",
                "10000",
                "11001",
                "00010",
                "01010"
                ],
            testResult=Just "198",
            testResult2=Just "230"
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

parse = map (map (== '1')) . lines

mostCommon :: [Bool] -> Bool
mostCommon column = (length $ filter id column)*2 >= length column

toInt :: [Bool] -> Int
toInt = foldl bit 0
  where
    bit n True = 1+2*n
    bit n False = 2*n

makeMostCommon :: [[Bool]] -> [Bool]
makeMostCommon rows
  | null $ head rows = []
  | otherwise = mostCommon (map head rows) : makeMostCommon (map tail rows)

result rows = toInt common * toInt (map not common)
  where common = makeMostCommon rows

findRating :: [[Bool]] -> (Bool -> Bool) -> Int
findRating report selector = search (map addRating report)
  where
    addRating bits = (toInt bits,bits)

    search [(rating,_)] = rating
    search ratings =
        search $ map (fmap tail) $ filter ((select ==) . head . snd) ratings
      where
        select = selector $ mostCommon $ map (head . snd) ratings

result2 report = findRating report id * findRating report not
