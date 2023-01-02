{-
--- Day 1: Inverse Captcha ---

The night before Christmas, one of Santa's Elves calls you in a panic. "The
printer's broken! We can't print the Naughty or Nice List!" By the time you
make it to sub-basement 17, there are only a few minutes until midnight. "We
have a big problem," she says; "there must be almost fifty bugs in this system,
but nothing else can print The List. Stand in this square, quick! There's no
time to explain; if you can convince them to pay you in stars, you'll be able
to--" She pulls a lever and the world goes blurry.

When your eyes can focus again, everything seems a lot more pixelated than
before. She must have sent you inside the computer! You check the system clock:
25 milliseconds until midnight. With that much time, you should be able to
collect all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each
day millisecond in the Advent calendar; the second puzzle is unlocked when you
complete the first. Each puzzle grants one star. Good luck!

You're standing in a room with "digitization quarantine" written in LEDs along
one wall. The only door is locked, but it includes a small interface.
"Restricted Area - Strictly No Digitized Users Allowed."

It goes on to explain that you may only leave by solving a captcha to prove
you're not a human. Apparently, you only get one millisecond to solve the
captcha: too fast for a normal human, but it feels like hours to you.

The captcha requires you to review a sequence of digits (your puzzle input) and
find the sum of all digits that match the next digit in the list. The list is
circular, so the digit after the last digit is the first digit in the list.

For example:

 - 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the
   second digit and the third digit (2) matches the fourth digit.
 - 1111 produces 4 because each digit (all 1) matches the next.
 - 1234 produces 0 because no digit matches the next.
 - 91212129 produces 9 because the only digit that matches the next one is the
   last digit, 9.

What is the solution to your captcha?
-}

import Data.Char(isDigit,ord)

parse :: String -> [Int]
parse s = map ((+ (-ord '0')) . ord) $ filter isDigit $ s

captcha :: [Int] -> Int
captcha digits = fst $ foldr scan (0,head digits) digits
  where
    scan digit (runningTotal,nextDigit)
      | digit == nextDigit = (digit+runningTotal,digit)
      | otherwise = (runningTotal,digit)

test :: ()
test
  | captcha (parse "1122") /= 3 = error "a"
  | captcha (parse "1111") /= 4 = error "b"
  | captcha (parse "1234") /= 0 = error "c"
  | captcha (parse "91212129") /= 9 = error "d"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (captcha . parse) $ readFile "input/01.txt"

captcha2 :: [Int] -> Int
captcha2 digits = sum $ map fst $ filter (uncurry (==)) $ zip digits (right++left)
  where (left,right) = splitAt (length digits `div` 2) digits

test2 :: ()
test2
  | captcha2 (parse "1212") /= 6 = error "a"
  | captcha2 (parse "1221") /= 0 = error "b"
  | captcha2 (parse "123425") /= 4 = error "c"
  | captcha2 (parse "123123") /= 12 = error "d"
  | captcha2 (parse "12131415") /= 4 = error "e"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (captcha2 . parse) $ readFile "input/01.txt"
