module AOC202307 where

import Data.List(sort)

import AOC

aoc = AOC {
    day="../../2023/input/07",
    aocTests=[
        AOCTest {
            testData=unlines [
                "32T3K 765",
                "T55J5 684",
                "KK677 28",
                "KTJJT 220",
                "QQQJA 483"
                ],
            testResult=Just "6440",
            testResult2=Just "5905"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result,
        codeResult=result,
        codeResult2=result
        }
    }

card '2' = 2
card '3' = 3
card '4' = 4
card '5' = 5
card '6' = 6
card '7' = 7
card '8' = 8
card '9' = 9
card 'T' = 10
card 'J' = 11
card 'Q' = 13
card 'K' = 14
card 'A' = 15

getType hand = g $ sort hand
  where
    g [a,b,c,d,e]
      | a == e = 7
      | a == d || b == e = 6
      | (a == c && d == e) || (a == b && c == e) = 5
      | a == c || b == d || c == e = 4
      | (a == b && c == d) || (a == b && d == e) || (b == c && d == e) = 3
      | a == b || b == c || c == d || d == e = 2
      | otherwise = 1

parse :: String -> [((Int,[Int]),Int)]
parse = map p . lines
  where p (a:b:c:d:e:' ':bid) = ((getType hand,hand),read bid)
          where hand = map card [a,b,c,d,e]

result = sum . zipWith (*) [1..] . map snd . sort

card2 'J' = 0
card2 c = card c

getType2 hand = g $ sort hand
  where
    g [a,b,c,d,e]
      | d == 0 = 7
      | c == 0 && d == e = 7
      | b == 0 && c == e = 7
      | a == 0 && b == e = 7
      | a == e = 7
      | c == 0 = 6
      | b == 0 && (c == d || d == e) = 6
      | a == 0 && (b == d || c == e) = 6
      | a == d || b == e = 6
      | b == 0 && (c == d || d == e) = 5
      | a == 0 && b == c && d == e = 5
      | (a == c && d == e) || (a == b && c == e) = 5
      | b == 0 = 4
      | a == 0 && (b == c || c == d || d == e) = 4
      | a == c || b == d || c == e = 4
      | (a == b && c == d) || (a == b && d == e) || (b == c && d == e) = 3
      | a == 0 = 2
      | a == b || b == c || c == d || d == e = 2
      | otherwise = 1

parse2 :: String -> [((Int,[Int]),Int)]
parse2 = map p . lines
  where p (a:b:c:d:e:' ':bid) = ((getType2 hand,hand),read bid)
          where hand = map card2 [a,b,c,d,e]
