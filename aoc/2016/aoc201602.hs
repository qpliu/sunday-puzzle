{-
--- Day 2: Bathroom Security ---

You arrive at Easter Bunny Headquarters under cover of darkness. However, you
left in such a rush that you forgot to use the bathroom! Fancy office buildings
like this one usually have keypad locks on their bathrooms, so you search the
front desk for the code.

"In order to improve security," the document you find says, "bathroom codes
will no longer be written down. Instead, please memorize and follow the
procedure below to access the bathrooms."

The document goes on to explain that each button to be pressed can be found by
starting on the previous button and moving to adjacent buttons on the keypad: U
moves up, D moves down, L moves left, and R moves right. Each line of
instructions corresponds to one button, starting at the previous button (or,
for the first line, the "5" button); press whatever button you're on at the end
of each line. If a move doesn't lead to a button, ignore it.

You can't hold it much longer, so you decide to figure out the code as you walk
to the bathroom. You picture a keypad like this:

1 2 3
4 5 6
7 8 9

Suppose your instructions are:

ULL
RRDDD
LURDL
UUUUD

 - You start at "5" and move up (to "2"), left (to "1"), and left (you can't,
   and stay on "1"), so the first button is 1.
 - Starting from the previous button ("1"), you move right twice (to "3") and
   then down three times (stopping at "9" after two moves and ignoring the
   third), ending up with 9.
 - Continuing from "9", you move left, up, right, down, and left, ending with
   8.
 - Finally, you move up four times (stopping at "2"), then down once, ending
   with 5.

So, in this example, the bathroom code is 1985.

Your puzzle input is the instructions from the document you found at the front desk. What is the bathroom code?
-}

import Data.Map(Map,fromList,member,(!))

makePad :: Map (Int,Int) Char
makePad = fromList [((1,1),'1'),((2,1),'2'),((3,1),'3'),((1,2),'4'),((2,2),'5'),((3,2),'6'),((1,3),'7'),((2,3),'8'),((3,3),'9')]

button :: Map (Int,Int) Char -> ((Int,Int),String) -> String -> ((Int,Int),String)
button pad ((x,y),revcode) "" = ((x,y),pad!(x,y):revcode)
button pad ((x,y),revcode) (insn:insns)
  | (newx,newy) `member` pad = button pad ((newx,newy),revcode) insns
  | otherwise = button pad ((x,y),revcode) insns
  where
    newx | insn == 'R' = x+1 | insn == 'L' = x-1 | otherwise = x
    newy | insn == 'U' = y-1 | insn == 'D' = y+1 | otherwise = y

follow :: String -> String
follow insns = reverse $ snd $ foldl (button makePad) ((2,2),"") (lines insns)

test :: ()
test
  | follow "ULL\nRRDDD\nLURDL\nUUUUD" /= "1985" = error "a"
  | otherwise = ()

part1 :: IO String
part1 = fmap follow $ readFile "input/02.txt"

makePad2 :: Map (Int,Int) Char
makePad2 = fromList [((3,1),'1'),((2,2),'2'),((3,2),'3'),((4,2),'4'),((1,3),'5'),((2,3),'6'),((3,3),'7'),((4,3),'8'),((5,3),'9'),((2,4),'A'),((3,4),'B'),((4,4),'C'),((3,5),'D')]

follow2 :: String -> String
follow2 insns = reverse $ snd $ foldl (button makePad2) ((1,3),"") (lines insns)

test2 :: ()
test2
  | follow2 "ULL\nRRDDD\nLURDL\nUUUUD" /= "5DB3" = error "a"
  | otherwise = ()

part2 :: IO String
part2 = fmap follow2 $ readFile "input/02.txt"
