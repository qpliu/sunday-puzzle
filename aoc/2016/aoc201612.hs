{-
--- Day 12: Leonardo's Monorail ---

You finally reach the top floor of this building: a garden with a slanted glass
ceiling. Looks like there are no more stars to be had.

While sitting on a nearby bench amidst some tiger lilies, you manage to decrypt
some of the files you extracted from the servers downstairs.

According to these documents, Easter Bunny HQ isn't just this building - it's a
collection of buildings in the nearby area. They're all connected by a local
monorail, and there's another building not far from here! Unfortunately, being
night, the monorail is currently not operating.

You remotely connect to the monorail control systems and discover that the boot
sequence expects a password. The password-checking logic (your puzzle input) is
easy to extract, but the code it uses is strange: it's assembunny code designed
for the new computer you just assembled. You'll have to execute the code and
get the password.

The assembunny code you've extracted operates on four registers (a, b, c, and
d) that start at 0 and can hold any integer. However, it seems to make use of
only a few instructions:

 - cpy x y copies x (either an integer or the value of a register) into
   register y.
 - inc x increases the value of register x by one.
 - dec x decreases the value of register x by one.
 - jnz x y jumps to an instruction y away (positive means forward; negative
   means backward), but only if x is not zero.

The jnz instruction moves relative to itself: an offset of -1 would continue at
the previous instruction, while an offset of 2 would skip over the next
instruction.

For example:

cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a

The above code would set register a to 41, increase its value by 2, decrease
its value by 1, and then skip the last dec a (because a is not zero, so the jnz
a 2 skips it), leaving register a at 42. When you move past the last
instruction, the program halts.

After executing the assembunny code in your puzzle input, what value is left in
register a?
-}

import Data.Map(Map,adjust,fromList,(!))

parse :: String -> ([[String]],[[String]])
parse input = (map words (lines input),[])

registers :: Map String Int
registers = fromList [("a",0),("b",0),("c",0),("d",0)]

isRegister :: String -> Bool
isRegister x = x `elem` ["a","b","c","d"]

interp :: Map String Int -> ([[String]],[[String]]) -> Map String Int
interp regs ([],_) = regs
interp regs (insn:forward,backward) = interp1 insn
  where
    interp1 ("cpy":x:y:_)
      | isRegister x = interp (adjust (const (regs!x)) y regs) (forward,insn:backward)
      | otherwise = interp (adjust (const (read x)) y regs) (forward,insn:backward)
    interp1 ("inc":x:_) = interp (adjust (+1) x regs) (forward,insn:backward)
    interp1 ("dec":x:_) = interp (adjust (-1+) x regs) (forward,insn:backward)
    interp1 ("jnz":x:y:_)
      | x == "0" || (isRegister x && regs!x == 0) = interp regs (forward,insn:backward)
      | offset < 0 = interp regs (reverse (take (-offset) backward) ++ insn:forward,drop (-offset) backward)
      | otherwise = interp regs (drop offset (insn:forward),reverse (take offset (insn:forward)) ++ backward)
      where offset | isRegister y = regs!y | otherwise = read y
    interp1 _ = error (show insn)

test :: ()
test
  | (interp registers (parse testData))!"a" /= 42 = error "a"
  | otherwise = ()
  where
    testData = "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"

part1 :: IO Int
part1 = fmap ((!"a") . interp registers . parse) $ readFile "input/12.txt"

part2registers :: Map String Int
part2registers = fromList [("a",0),("b",0),("c",1),("d",0)]

part2 :: IO Int
part2 = fmap ((!"a") . interp part2registers . parse) $ readFile "input/12.txt"
