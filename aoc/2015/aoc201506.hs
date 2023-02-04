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
