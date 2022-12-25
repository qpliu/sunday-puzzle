{-
--- Day 10: Elves Look, Elves Say ---

Today, the Elves are playing a game called look-and-say. They take turns making
sequences by reading aloud the previous sequence and using that reading as the
next sequence. For example, 211 is read as "one two, two ones", which becomes
1221 (1 2, 2 1s).

Look-and-say sequences are generated iteratively, using the previous value as
input for the next step. For each step, take the previous value, and replace
each run of digits (like 111) with the number of digits (3) followed by the
digit itself (1).

For example:

 - 1 becomes 11 (1 copy of digit 1).
 - 11 becomes 21 (2 copies of digit 1).
 - 21 becomes 1211 (one 2 followed by one 1).
 - 1211 becomes 111221 (one 1, one 2, and two 1s).
 - 111221 becomes 312211 (three 1s, two 2s, and one 1).

Starting with the digits in your puzzle input, apply this process 40 times.
What is the length of the result?
-}
import Data.List(group)

lookAndSay :: String -> String
lookAndSay s = concatMap say (group s)
  where
    say g@(c:_) = show (length g) ++ [c]

test :: ()
test
  | i 1 /= "11" = error "a"
  | i 2 /= "21" = error "b"
  | i 3 /= "1211" = error "c"
  | i 4 /= "111221" = error "d"
  | i 5 /= "312211" = error "e"
  | otherwise = ()
  where
    i n = head $ drop n $ iterate lookAndSay "1"

part1 :: String -> Int
part1 input = length $ head $ drop 40 $ iterate lookAndSay input

part2 :: String -> Int
part2 input = length $ head $ drop 50 $ iterate lookAndSay input
