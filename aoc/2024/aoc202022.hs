module AOC202022 where

import Data.Set(Set,empty,insert,member)

import AOC

aoc = AOC {
    day="../../2020/input/22",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Player 1:",
                "9",
                "2",
                "6",
                "3",
                "1",
                "",
                "Player 2:",
                "5",
                "8",
                "4",
                "7",
                "10"
                ],
            testResult=Just "306",
            testResult2=Just "291"
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

parse :: String -> [[Int]]
parse = map (map read . tail) . parseBlankLineSeparated

score :: [Int] -> Int
score = sum . zipWith (*) [1..] . reverse

play :: [Int] -> [Int] -> Int
play [] deck = score deck
play deck [] = score deck
play (c1:deck1) (c2:deck2)
  | c1 > c2 = play (deck1 ++ [c1,c2]) deck2
  | otherwise = play deck1 (deck2 ++ [c2,c1])

result (d1:d2:_) = play d1 d2

play2 :: Set ([Int],[Int]) -> ([Int],[Int]) -> (Bool,Int)
play2 played decks@([],deck2) = (False,score deck2)
play2 played decks@(deck1,[]) = (True,score deck1)
play2 played decks@(c1:deck1,c2:deck2)
  | member decks played = (True,score (c1:deck1))
  | player1wins = play2 (insert decks played) (deck1++[c1,c2],deck2)
  | otherwise = play2 (insert decks played) (deck1,deck2++[c2,c1])
  where
    player1wins
      | c1 <= length deck1 && c2 <= length deck2 =
            fst $ play2 empty (take c1 deck1,take c2 deck2)
      | otherwise = c1 > c2

result2 (d1:d2:_) = snd $ play2 empty (d1,d2)
