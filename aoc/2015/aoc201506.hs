{-
--- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating
contest year after year, you've decided to deploy one million lights in a
1000x1000 grid.

Furthermore, because you've been especially nice this year, Santa has mailed
you instructions on how to display the ideal lighting configuration.

Lights in your grid are numbered from 0 to 999 in each direction; the lights at
each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include
whether to turn on, turn off, or toggle various inclusive ranges given as
coordinate pairs. Each coordinate pair represents opposite corners of a
rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers
to 9 lights in a 3x3 square. The lights all start turned off.

To defeat your neighbors this year, all you have to do is set up your lights by
doing the instructions Santa sent you in order.

For example:

 - turn on 0,0 through 999,999 would turn on (or leave on) every light.
 - toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning
   off the ones that were on, and turning on the ones that were off.
 - turn off 499,499 through 500,500 would turn off (or leave off) the middle
   four lights.

After following the instructions, how many lights are lit?
-}

-- This code is kind of slow.

import Data.Array(Array,array,range,(!),(//))
import Data.Set(Set,difference,fromList,intersection,size,toList,union)

make :: (Int,Int) -> (Int,Int) -> Set (Int,Int)
make (x1,y1) (x2,y2) = fromList [(x,y) | x <- [x1..x2], y <- [y1..y2]]

interpret :: Set (Int,Int) -> String -> Set (Int,Int)
interpret grid instruction = interp (words instruction)
  where
    interp ("turn":"on":g1:"through":g2:_) = grid `union` (makeGrid g1 g2)
    interp ("toggle":g1:"through":g2:_) = let g = makeGrid g1 g2 in (grid `union` g) `difference` (grid `intersection` g)
    interp ("turn":"off":g1:"through":g2:_) = grid `difference` (makeGrid g1 g2)
    interp _ = error ("unrecognized instruction:"++instruction)
    makeGrid g1 g2 = make (read ("("++g1++")")) (read ("("++g2++")"))

instructions :: String -> Set (Int,Int)
instructions insns = foldl interpret (fromList []) (lines insns)

test :: ()
test
  | size (instructions "turn on 0,0 through 999,999") /= 1000000 = error "a"
  | size (instructions "toggle 0,0 through 999,0") /= 1000 = error "b"
  | size (instructions "turn off 499,499 through 500,500") /= 0 = error "c"
  | size (instructions "turn on 0,0 through 999,999\ntoggle 0,0 through 999,0\nturn off 499,499 through 500,500") /= 998996 = error "d"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (size . instructions) (readFile "input/06.txt")

part2instructions :: String -> Array (Int,Int) Int
part2instructions insns = foldl interp2 (array ((0,0),(999,999)) $ zip (range ((0,0),(999,999))) (repeat 0)) (lines insns)

interp2 :: Array (Int,Int) Int -> String -> Array (Int,Int) Int
interp2 grid instruction = interp (words instruction)
  where
    interp ("turn":"on":g1:"through":g2:_) = inc 1 (makeGrid g1 g2)
    interp ("toggle":g1:"through":g2:_) = inc 2 (makeGrid g1 g2)
    interp ("turn":"off":g1:"through":g2:_) = inc (-1) (makeGrid g1 g2)
    interp _ = error ("unrecognized instruction:"++instruction)
    makeGrid g1 g2 = make (read ("("++g1++")")) (read ("("++g2++")"))
    inc n g = grid // map update (toList g)
      where
        update coord = (coord,max 0 (n+grid!coord))

part2 :: IO Int
part2 = fmap (sum . part2instructions) (readFile "input/06.txt")
