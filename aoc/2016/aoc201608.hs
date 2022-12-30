{-
--- Day 8: Two-Factor Authentication ---

You come across a door implementing what you can only assume is an
implementation of two-factor authentication after a long game of requirements
telephone.

To get past the door, you first swipe a keycard (no problem; there was one on a
nearby desk). Then, it displays a code on a little screen, and you type that
code on a keypad. Then, presumably, the door unlocks.

Unfortunately, the screen has been smashed. After a few minutes, you've taken
everything apart and figured out how it works. Now you just have to work out
what the screen would have displayed.

The magnetic strip on the card you swiped encodes a series of instructions for
the screen; these instructions are your puzzle input. The screen is 50 pixels
wide and 6 pixels tall, all of which start off, and is capable of three
somewhat peculiar operations:

 - rect AxB turns on all of the pixels in a rectangle at the top-left of the
   screen which is A wide and B tall.
 - rotate row y=A by B shifts all of the pixels in row A (0 is the top row)
   right by B pixels. Pixels that would fall off the right end appear at the
   left end of the row.
 - rotate column x=A by B shifts all of the pixels in column A (0 is the left
   column) down by B pixels. Pixels that would fall off the bottom appear at
   the top of the column.

For example, here is a simple sequence on a smaller screen:

rect 3x2 creates a small rectangle in the top-left corner:

###....
###....
.......

rotate column x=1 by 1 rotates the second column down by one pixel:

#.#....
###....
.#.....

rotate row y=0 by 4 rotates the top row right by four pixels:

....#.#
###....
.#.....

rotate column x=1 by 1 again rotates the second column down by one pixel,
causing the bottom pixel to wrap back to the top:

.#..#.#
#.#....
.#.....

As you can see, this display technology is extremely powerful, and will soon
dominate the tiny-code-displaying-screen market. That's what the advertisement
on the back of the display tries to convince you, anyway.

There seems to be an intermediate check of the voltage used by the display:
after you swipe your card, if the screen did work, how many pixels should be
lit?
-}

import Data.Char(isDigit)
import Data.Set(Set,empty,fromList,member,size,union)
import qualified Data.Set

interpret :: (Int,Int) -> Set (Int,Int) -> String -> Set (Int,Int)
interpret dims@(w,h) set insn = interp (words insn)
  where
    interp ("rect":wxh:_) = fromList [(x,y) | x <- [0 .. min w (read (takeWhile isDigit wxh)) - 1], y <- [0 .. min h (read (takeWhile isDigit $ dropWhile (not . isDigit) $ dropWhile isDigit wxh)) - 1]] `union` set
    interp ("rotate":"column":x:"by":n:_) = Data.Set.map (rotcol (read $ filter isDigit x) (read n)) set
    interp ("rotate":"row":y:"by":n:_) = Data.Set.map (rotrow (read $ filter isDigit y) (read n)) set
    interp _ = error ("unrecognized:"++insn)
    rotcol col by (x,y) | x == col = (x,(y+by)`mod`h) | otherwise = (x,y)
    rotrow row by (x,y) | y == row = ((x+by)`mod`w,y) | otherwise = (x,y)

parse :: String -> Set (Int,Int)
parse s = fromList $ map fst $ filter ((== '#') . snd) $ concat $ zipWith parseRow [0..] $ lines s
  where
    parseRow row str = zip (map (flip (,) row) [0..]) str

test :: ()
test
  | interpret dim empty "rect 3x2" /= step1 = error "a"
  | interpret dim step1 "rotate column x=1 by 1" /= step2 = error "b"
  | interpret dim step2 "rotate row y=0 by 4" /= step3 = error "c"
  | interpret dim step3 "rotate column x=1 by 1" /= step4 = error "d"
  | otherwise = ()
  where
    dim = (7,3)
    step1 = parse "###....\n###....\n......."
    step2 = parse "#.#....\n###....\n.#....."
    step3 = parse "....#.#\n###....\n.#....."
    step4 = parse ".#..#.#\n#.#....\n.#....."

part1 :: IO Int
part1 = fmap (size . foldl (interpret (50,6)) empty . lines) $ readFile "input/08.txt"

display :: (Int,Int) -> Set (Int,Int) -> String
display (w,h) set = unlines [[if (x,y) `member` set then '#' else '.' | x <- [0..w-1]]| y <- [0..h-1]]

part2 :: IO ()
part2 = readFile "input/08.txt" >>= putStrLn . display (50,6) . foldl (interpret (50,6)) empty . lines
