module AOC202205 where

import AOC

aoc = AOC {
    day="../../2022/input/05",
    aocTests=[
        AOCTest {
            testData=unlines [
                "    [D]    ",
                "[N] [C]    ",
                "[Z] [M] [P]",
                " 1   2   3 ",
                "",
                "move 1 from 2 to 1",
                "move 3 from 1 to 3",
                "move 2 from 2 to 1",
                "move 1 from 1 to 2"
                ],
            testResult=Just "\"CMZ\"",
            testResult2=Just "\"MCD\""
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result reverse,
        codeTest2=result id,
        codeResult=result reverse,
        codeResult2=result id
        }
    }

parse = p . lines
  where
    p inp = (parseStacks (reverse stacks),map parseInts $ dropWhile null rest)
      where (stacks,rest) = span (not . null) inp

    parseStacks (labels:rest) =
        map (filter (/= ' ')) $ foldl parseLayer initialStacks rest
      where initialStacks = (replicate (length $ words labels) [])

    parseLayer stacks layer = zipWith (:) [layer!!(1+i*4) | i <- [0..]] stacks

rearrange :: (String -> String) -> [String] -> [Int] -> [String]
rearrange reorder stacks [n,from,to] = map move $ zip [1..] stacks
  where
    crates = reorder $ take n $ stacks!!(from-1)
    move (i,stack)
     | i == from = drop n stack
     | i == to = crates ++ stack
     | otherwise = stack

result reorder (stacks,steps) =
    map head $ foldl (rearrange reorder) stacks steps
