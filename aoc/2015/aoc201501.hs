{-
--- Day 1: Not Quite Lisp ---

Santa was hoping for a white Christmas, but his weather machine's "snow"
function is powered by stars, and he's fresh out! To save Christmas, he needs
you to collect fifty stars by December 25th.

Collect stars by helping Santa solve puzzles. Two puzzles will be made
available on each day in the Advent calendar; the second puzzle is unlocked
when you complete the first. Each puzzle grants one star. Good luck!

Here's an easy puzzle to warm you up.

Santa is trying to deliver presents in a large apartment building, but he can't
find the right floor - the directions he got are a little confusing. He starts
on the ground floor (floor 0) and then follows the instructions one character
at a time.

An opening parenthesis, (, means he should go up one floor, and a closing
parenthesis, ), means he should go down one floor.

The apartment building is very tall, and the basement is very deep; he will
never find the top or bottom floors.

For example:

 - (()) and ()() both result in floor 0.
 - ((( and (()(()( both result in floor 3.
 - ))((((( also results in floor 3.
 - ()) and ))( both result in floor -1 (the first basement level).
 - ))) and )())()) both result in floor -3.

To what floor do the instructions take Santa?
-}

instruction :: Integer -> Char -> Integer
instruction floor insn
  | insn == '(' = floor + 1
  | insn == ')' = floor - 1
  | otherwise = floor

instructions :: Integer -> String -> Integer
instructions floor = foldl instruction floor

test :: ()
test
  | instructions 0 "(())" /= 0 = error "a"
  | instructions 0 "()()" /= 0 = error "b"
  | instructions 0 "(((" /= 3 = error "c"
  | instructions 0 "(()(()(" /= 3 = error "d"
  | instructions 0 "))(((((" /= 3 = error "e"
  | instructions 0 "())" /= -1 = error "f"
  | instructions 0 "))(" /= -1 = error "g"
  | instructions 0 ")))" /= -3 = error "h"
  | instructions 0 ")())())" /= -3 = error "i"
  | otherwise = ()

part1 :: IO Integer
part1 = fmap (instructions 0) (readFile "input/01.txt")

part2instructions :: Integer -> Integer -> String -> Integer
part2instructions floor position (insn:insns)
  | floor < 0 = position
  | otherwise = part2instructions (instruction floor insn) (position+1) insns

part2 :: IO Integer
part2 = fmap (part2instructions 0 0) (readFile "input/01.txt")
