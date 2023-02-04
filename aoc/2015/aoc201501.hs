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
