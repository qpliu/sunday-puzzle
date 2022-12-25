{-
--- Day 3: Perfectly Spherical Houses in a Vacuum ---

Santa is delivering presents to an infinite two-dimensional grid of houses.

He begins by delivering a present to the house at his starting location, and
then an elf at the North Pole calls him via radio and tells him where to move
next. Moves are always exactly one house to the north (^), south (v), east (>),
or west (<). After each move, he delivers another present to the house at his
new location.

However, the elf back at the north pole has had a little too much eggnog, and
so his directions are a little off, and Santa ends up visiting some houses more
than once. How many houses receive at least one present?

For example:

 - > delivers presents to 2 houses: one at the starting location, and one to
   the east.
 - ^>v< delivers presents to 4 houses in a square, including twice to the house
   at his starting/ending location.
 - ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only
   2 houses.
-}
import Data.Set(Set,fromList,insert,size)

deliver :: ((Integer,Integer),Set (Integer,Integer)) -> Char -> ((Integer,Integer),Set (Integer,Integer))
deliver ((x,y),set) insn
  | insn == '^' = ((x,y-1),insert (x,y-1) set)
  | insn == 'v' = ((x,y+1),insert (x,y+1) set)
  | insn == '<' = ((x-1,y),insert (x-1,y) set)
  | insn == '>' = ((x+1,y),insert (x+1,y) set)
  | otherwise = ((x,y),set)

deliveries :: String -> Int
deliveries insns = size $ snd $ foldl deliver ((0,0),fromList [(0,0)]) insns

main :: IO ()
main = getContents >>= print . deliveries

test :: ()
test
  | deliveries ">" /= 2 = error "a"
  | deliveries "^>v<" /= 4 = error "b"
  | deliveries "^v^v^v^v^v" /= 2 = error "c"
  | otherwise = ()

part1 :: IO Int
part1 = fmap deliveries (readFile "input/03.txt")

part2a :: ((Integer,Integer),(Integer,Integer),Set (Integer,Integer)) -> Char -> ((Integer,Integer),(Integer,Integer),Set (Integer,Integer))
part2a ((x,y),b,set) insn
  | insn == '^' = ((x,y-1),b,insert (x,y-1) set)
  | insn == 'v' = ((x,y+1),b,insert (x,y+1) set)
  | insn == '<' = ((x-1,y),b,insert (x-1,y) set)
  | insn == '>' = ((x+1,y),b,insert (x+1,y) set)
  | otherwise = ((x,y),b,set)

part2b :: ((Integer,Integer),(Integer,Integer),Set (Integer,Integer)) -> Char -> ((Integer,Integer),(Integer,Integer),Set (Integer,Integer))
part2b (a,(x,y),set) insn
  | insn == '^' = (a,(x,y-1),insert (x,y-1) set)
  | insn == 'v' = (a,(x,y+1),insert (x,y+1) set)
  | insn == '<' = (a,(x-1,y),insert (x-1,y) set)
  | insn == '>' = (a,(x+1,y),insert (x+1,y) set)
  | otherwise = (a,(x,y),set)

part2c :: String -> Int
part2c insns = size $ runa ((0,0),(0,0),fromList [(0,0)]) insns
  where
    runa (_,_,set) "" = set
    runa state (insn:insns) = runb (part2a state insn) insns
    runb (_,_,set) "" = set
    runb state (insn:insns) = runa (part2b state insn) insns

part2 :: IO Int
part2 = fmap part2c (readFile "input/03.txt")
