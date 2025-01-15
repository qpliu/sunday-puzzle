module AOC202110 where

import Data.List(sort)

import AOC

aoc = AOC {
    day="../../2021/input/10",
    aocTests=[
        AOCTest {
            testData=unlines [
                "[({(<(())[]>[[{[]{<()<>>",
                "[(()[<>])]({[<{<<[]>>(",
                "{([(<{}[<>[]}>{[]{[(<()>",
                "(((({<>}<{<{<>}{[]{[]{}",
                "[[<[([]))<([[{}[[()]]]",
                "[{[{({}]{}}([{[{{{}}([]",
                "{<[[]]>}<{[{[{[]{()[[[]",
                "[<(<(<(<{}))><([]([]()",
                "<{([([[(<>()){}]>(<<{{",
                "<{([{{}}[<[[[<>{}]]]>[]]"
                ],
            testResult=Just "26397",
            testResult2=Just "288957"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const lines,
        pcodeParse2=const lines,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

score :: String -> String -> Int
score _ [] = 0
score stack line
  | take 1 stack == take 1 line = score (drop 1 stack) (drop 1 line)
score _ (')':_) = 3
score _ (']':_) = 57
score _ ('}':_) = 1197
score _ ('>':_) = 25137
score stack ('(':rest) = score (')':stack) rest
score stack ('[':rest) = score (']':stack) rest
score stack ('{':rest) = score ('}':stack) rest
score stack ('<':rest) = score ('>':stack) rest

result ncpu = parallelMapReduce ncpu (score []) sum

autocomplete :: String -> String -> Int
autocomplete stack [] = score2 0 stack
autocomplete stack line
  | take 1 stack == take 1 line = autocomplete (drop 1 stack) (drop 1 line)
autocomplete _ (')':_) = -1
autocomplete _ (']':_) = -1
autocomplete _ ('}':_) = -1
autocomplete _ ('>':_) = -1
autocomplete stack ('(':rest) = autocomplete (')':stack) rest
autocomplete stack ('[':rest) = autocomplete (']':stack) rest
autocomplete stack ('{':rest) = autocomplete ('}':stack) rest
autocomplete stack ('<':rest) = autocomplete ('>':stack) rest

score2 :: Int -> String -> Int
score2 n [] = n
score2 n (')':rest) = score2 (n*5+1) rest
score2 n (']':rest) = score2 (n*5+2) rest
score2 n ('}':rest) = score2 (n*5+3) rest
score2 n ('>':rest) = score2 (n*5+4) rest

median :: [Int] -> Int
median scores = head $ drop (length scores `div` 2) $ sort scores

result2 ncpu = median . filter (>= 0) . parallelMap ncpu (autocomplete [])
