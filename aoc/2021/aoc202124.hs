{-
--- Day 24: Arithmetic Logic Unit ---

Magic smoke starts leaking from the submarine's arithmetic logic unit (ALU).
Without the ability to perform basic arithmetic and logic functions, the
submarine can't produce cool patterns with its Christmas lights!

It also can't navigate. Or run the oxygen system.

Don't worry, though - you probably have enough oxygen left to give you enough
time to build a new ALU.

The ALU is a four-dimensional processing unit: it has integer variables w, x,
y, and z. These variables all start with the value 0. The ALU also supports six
instructions:

 - inp a - Read an input value and write it to variable a.
 - add a b - Add the value of a to the value of b, then store the result in
   variable a.
 - mul a b - Multiply the value of a by the value of b, then store the result
   in variable a.
 - div a b - Divide the value of a by the value of b, truncate the result to an
   integer, then store the result in variable a. (Here, "truncate" means to
   round the value toward zero.)
 - mod a b - Divide the value of a by the value of b, then store the remainder
   in variable a. (This is also called the modulo operation.)
 - eql a b - If the value of a and b are equal, then store the value 1 in
   variable a. Otherwise, store the value 0 in variable a.

In all of these instructions, a and b are placeholders; a will always be the
variable where the result of the operation is stored (one of w, x, y, or z),
while b can be either a variable or a number. Numbers can be positive or
negative, but will always be integers.

The ALU has no jump instructions; in an ALU program, every instruction is run
exactly once in order from top to bottom. The program halts after the last
instruction has finished executing.

(Program authors should be especially cautious; attempting to execute div with
b=0 or attempting to execute mod with a<0 or b<=0 will cause the program to
crash and might even damage the ALU. These operations are never intended in any
serious ALU program.)

For example, here is an ALU program which takes an input number, negates it,
and stores it in x:

| inp x
| mul x -1

Here is an ALU program which takes two input numbers, then sets z to 1 if the
second input number is three times larger than the first input number, or sets
z to 0 otherwise:

| inp z
| inp x
| mul z 3
| eql z x

Here is an ALU program which takes a non-negative integer as input, converts it
into binary, and stores the lowest (1's) bit in z, the second-lowest (2's) bit
in y, the third-lowest (4's) bit in x, and the fourth-lowest (8's) bit in w:

| inp w
| add z w
| mod z 2
| div w 2
| add y w
| mod y 2
| div w 2
| add x w
| mod x 2
| div w 2
| mod w 2

Once you have built a replacement ALU, you can install it in the submarine,
which will immediately resume what it was doing when the ALU failed: validating
the submarine's model number. To do this, the ALU will run the MOdel Number
Automatic Detector program (MONAD, your puzzle input).

Submarine model numbers are always fourteen-digit numbers consisting only of
digits 1 through 9. The digit 0 cannot appear in a model number.

When MONAD checks a hypothetical fourteen-digit model number, it uses fourteen
separate inp instructions, each expecting a single digit of the model number in
order of most to least significant. (So, to check the model number
13579246899999, you would give 1 to the first inp instruction, 3 to the second
inp instruction, 5 to the third inp instruction, and so on.) This means that
when operating MONAD, each input instruction should only ever be given an
integer value of at least 1 and at most 9.

Then, after MONAD has finished running all of its instructions, it will
indicate that the model number was valid by leaving a 0 in variable z. However,
if the model number was invalid, it will leave some other non-zero value in z.

MONAD imposes additional, mysterious restrictions on model numbers, and legend
says the last copy of the MONAD documentation was eaten by a tanuki. You'll
need to figure out what MONAD does some other way.

To enable as many submarine features as possible, find the largest valid
fourteen-digit model number that contains no 0 digits. What is the largest
model number accepted by MONAD?
-}

import Data.Char(chr)
import Data.List(sort)

abcs :: [(Int,Int,Int)]
abcs = [(1,13,3),(1,11,12),(1,15,9),(26,-6,12),(1,15,2),(26,-8,1),(26,-4,1),(1,15,13),(1,10,1),(1,11,6),(26,-11,2),(26,0,11),(26,-8,10),(26,-7,3)]

-- Decompiling the code gives:
-- if input == b + z mod 26
-- then z = (z div a)*26 + input + c
-- else z = (z div a)
monad1 :: Int -> (Int,(Int,Int,Int)) -> Int
monad1 z (i,(a,b,c))
  | i /= b + z `mod` 26 = (z `div` a)*26 + i + c
  | otherwise = z `div` a

monad :: [Int] -> Int
monad input = foldl monad1 0 $ zip input abcs

-- Coming into the 14th digit,
-- Need the 14th digit + 7 = z mod 26, and z < 26.
-- Write z in base 26.
-- Need z = (8..16).

-- Coming into the 13th digit, need to have z come out (8..16),
-- z = (8..16,0..25) -- ie. (8..16)*26 + (0..25)
-- Need the 13th digit + 8 = z mod 26.
-- Need z = (8..16,9..17) -- ie. (8..16)*26 + (9..17)

-- Coming into the 12th digit, need to have z come out (8..16,9..17),
-- z = (8..16,9..17,0..25)
-- Need the 12th digit = z mod 26
-- Need z = (8..16,9..17,0..9)
--
-- Coming into the 11th digit, need to have z come out (8..16,9..17,0..9)
-- z = (8..16,9..17,0..9,0..25)
-- Need the 11th digit + 11 = z mod 26
-- Need z = (8..16,9..17,0..9,12..20)
--
-- Coming into the 10th digit, need to have z come out (8..16,9..17,0..9,12..20)
-- z = (8..16,9..17,0..9,7..15), making 10th digit 9 and 11th digit 4
-- Need z = (8..16,9..17,0..9), note that 0..9 does not interfere with 9(+11).
--
-- Coming into the 9th digit, need to have z come out (8..16,9..17,0..9)
-- z = (8..16,9..17,2..10), making 9th digit 8 and 12th digit 9
-- Need z = (8..16,9..17) -- note that 9..17 does not interfere with 8(+10).
--
-- Coming into the 8th digit, need to have z come out (8..16,9..17)
-- z = (8..16,14..22), making the 8th digit 4 and the 13th digit 9
-- Need z = (8..16) -- note that 8..16 does not interfere with 4(+15).
--
-- Coming into the 7th digit, need to have z come out (8..16)
-- Need z = (8..16,5..14)
--
-- Coming into the 6th digit, need to have z come out (8..16,5..14)
-- Need z = (8..16,5..14,9..17)
--
-- Coming into the 5th digit, need to have z come out (8..16,5..14,9..17)
-- z = (8..16,5..14,3..10), making the 5th digit 9 and the 6th digit 2
-- Need z = (8..16,5..14) -- 5..14 does not interfere with 9(+15)
--
-- Coming into the 4th digit, need to have z come out (8..16,5..14)
-- Need z = (8..16,5..14,7..15)
--
-- Coming into the 3rd digit, need to have z come out (8..16,5..14,7..15)
-- z = (8..16,5..14,10..19), making the 3rd digit 6 and the 4th digit 9
-- Need z = (8..16,5..14) -- 5..14 does not interfere with 6(+15)
--
-- Coming into the 2nd digit, need to have z come out (8..16,5..14)
-- z = (8..16,13..21), making the 2nd digit 2 and the 7th digit 9
-- Need z = (8..16) -- 8..16 DOES interfere with 2(+12)
-- Need z = (8..13+15..16)
--
-- Coming into the 1st digit, need to have z come out (8..13+15..16)
-- z = (4..12), making the 1st digit 9 and the 14th digit 5
--
-- Got some of the digits wrong, but the code gets them all right.

search :: [(Int,Int,Int)] -> [(Int,(Int,Int))] -> [(Int,Int)]
search [] _ = []
search all@((a,b,c):rest) z
  | a == 26 = search rest ((currentDigit,(1-b,9-b)):z)
  | otherwise = (currentDigit,min 9 (stackHigh-c)):(stackDigit,min 9 (c+10-stackLow)) : search rest stack
  where
    currentDigit = length all
    ((stackDigit,(stackLow,stackHigh)):stack) = z

runSearch :: [Int]
runSearch = map snd $ sort $ search (reverse abcs) []

part1 :: String
part1 = map (chr . (+48)) runSearch

search2 :: [(Int,Int,Int)] -> [(Int,(Int,Int))] -> [(Int,Int)]
search2 [] _ = []
search2 all@((a,b,c):rest) z
  | a == 26 = search2 rest ((currentDigit,(1-b,9-b)):z)
  | otherwise = (currentDigit,max 1 (stackLow-c)):(stackDigit,max 1 (c+10-stackHigh)) : search2 rest stack
  where
    currentDigit = length all
    ((stackDigit,(stackLow,stackHigh)):stack) = z

runSearch2 :: [Int]
runSearch2 = map snd $ sort $ search2 (reverse abcs) []

part2 :: String
part2 = map (chr . (+48)) runSearch2
