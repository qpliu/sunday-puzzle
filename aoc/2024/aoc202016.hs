module AOC202016 where

import Data.Set(Set,delete,fromList,size)

import AOC

aoc = AOC {
    day="../../2020/input/16",
    aocTests=[
        AOCTest {
            testData=unlines [
                "class: 1-3 or 5-7",
                "row: 6-11 or 33-44",
                "seat: 13-40 or 45-50",
                "",
                "your ticket:",
                "7,1,14",
                "",
                "nearby tickets:",
                "7,3,47",
                "40,4,50",
                "55,2,20",
                "38,6,12"
                ],
            testResult=Just "71",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "class: 0-1 or 4-19",
                "row: 0-5 or 8-19",
                "seat: 0-13 or 16-19",
                "",
                "your ticket:",
                "11,12,13",
                "",
                "nearby tickets:",
                "3,9,18",
                "15,1,5",
                "5,14,9"
                ],
            testResult=Nothing,
            testResult2=Just (show $ show ["row","class","seat"])
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=show . columns,
        codeResult=result,
        codeResult2=show . result2
        }
    }

parse = p . parseBlankLineSeparated
  where
    p [rules,["your ticket:",yours],("nearby tickets:":nearby)] =
        (map parseRule rules,parseInts yours,map parseInts nearby)
    parseRule rule = (field,validate $ map abs $ parseInts ranges)
      where
        (field,ranges) = span (/= ':') rule
    validate [a,b,c,d] v = (a <= v && v <= b) || (c <= v && v <= d)

result (rules,_,nearby) = sum $ filter invalid $ concat nearby
  where
    validators = map snd rules
    invalid n = not $ any ($ n) validators

validTicket :: [Int -> Bool] -> [Int] -> Bool
validTicket rules = all validField
  where validField n = any ($ n) rules

columns :: ([(String,Int -> Bool)],[Int],[[Int]]) -> [String]
columns (rules,_,nearby) = eliminate candidates initialQueue
  where
    valid :: [[Int]]
    valid = filter (validTicket (map snd rules)) nearby

    candidates :: [Set String]
    candidates = map (fromList . map fst) $ foldr sieve (repeat rules) valid

    sieve :: [Int] -> [[(String,Int -> Bool)]] -> [[(String,Int -> Bool)]]
    sieve ticket rules = zipWith sieveField ticket rules

    sieveField :: Int -> [(String,Int -> Bool)] -> [(String,Int -> Bool)]
    sieveField value rules = filter (($ value) . snd) rules

    initialQueue :: [String]
    initialQueue = map minimum $ filter ((== 1) . size) candidates

    eliminate :: [Set String] -> [String] -> [String]
    eliminate fields [] = map minimum fields
    eliminate fields (field:queue) = eliminate nextFields nextQueue
      where
        (nextFields,nextQueue) = eliminate1 [] queue fields

        eliminate1 passFields passQueue [] = (reverse passFields,passQueue)
        eliminate1 passFields passQueue (f:fs)
          | size f == 1 = eliminate1 (f:passFields) passQueue fs
          | size nextF == 1 =
              eliminate1 (nextF:passFields) (minimum nextF:passQueue) fs
          | otherwise = eliminate1 (nextF:passFields) passQueue fs
          where nextF = delete field f

result2 input@(_,yours,_) =
    product $ map fst $ filter ((== "departure") . take 9 . snd)
                      $ zip yours $ columns input
