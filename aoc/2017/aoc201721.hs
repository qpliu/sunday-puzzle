{-
--- Day 21: Fractal Art ---

You find a program trying to generate some art. It uses a strange process that
involves repeatedly enhancing the detail of an image through a set of rules.

The image consists of a two-dimensional square grid of pixels that are either
on (#) or off (.). The program always begins with this pattern:

| .#.
| ..#
| ###

Because the pattern is both 3 pixels wide and 3 pixels tall, it is said to have
a size of 3.

Then, the program repeats the following process:

 - If the size is evenly divisible by 2, break the pixels up into 2x2 squares,
   and convert each 2x2 square into a 3x3 square by following the corresponding
   enhancement rule.
 - Otherwise, the size is evenly divisible by 3; break the pixels up into 3x3
   squares, and convert each 3x3 square into a 4x4 square by following the
   corresponding enhancement rule.

Because each square of pixels is replaced by a larger one, the image gains
pixels and so its size increases.

The artist's book of enhancement rules is nearby (your puzzle input); however,
it seems to be missing rules. The artist explains that sometimes, one must
rotate or flip the input pattern to find a match. (Never rotate or flip the
output pattern, though.) Each pattern is written concisely: rows are listed as
single units, ordered top-down, and separated by slashes. For example, the
following rules correspond to the adjacent patterns:

| ../.#  =  ..
|           .#
| 
|                 .#.
| .#./..#/###  =  ..#
|                 ###
| 
|                         #..#
| #..#/..../#..#/.##.  =  ....
|                         #..#
|                         .##.

When searching for a rule to use, rotate and flip the pattern as necessary. For
example, all of the following patterns match the same rule:

| .#.   .#.   #..   ###
| ..#   #..   #.#   ..#
| ###   ###   ##.   .#.

Suppose the book contained the following two rules:

| ../.# => ##./#../...
| .#./..#/### => #..#/..../..../#..#

As before, the program begins with this pattern:

| .#.
| ..#
| ###

The size of the grid (3) is not divisible by 2, but it is divisible by 3. It
divides evenly into a single square; the square matches the second rule, which
produces:

| #..#
| ....
| ....
| #..#

The size of this enhanced grid (4) is evenly divisible by 2, so that rule is
used. It divides evenly into four squares:

| #.|.#
| ..|..
| --+--
| ..|..
| #.|.#

Each of these squares matches the same rule (../.# => ##./#../...), three of
which require some flipping and rotation to line up with the rule. The output
for the rule is the same in all four cases:

| ##.|##.
| #..|#..
| ...|...
| ---+---
| ##.|##.
| #..|#..
| ...|...

Finally, the squares are joined into a new grid:

| ##.##.
| #..#..
| ......
| ##.##.
| #..#..
| ......

Thus, after 2 iterations, the grid contains 12 pixels that are on.

How many pixels stay on after 5 iterations?
-}

-- Iteration 1: 3x3 -> 4x4
-- Iteration 2: 4x4 = 2x2(2x2) -> 2x2(3x3) = 6x6
-- Iteration 3: 6x6 = 3x3(2x2) -> 3x3(3x3) = 9x9
-- Iteration 4: 9x9 = 3x3(3x3) -> 3x3(4x4) = 12x12
-- Iteration 5: 12x12 = 6x6(2x2) -> 6x6(3x3) = 18x18

import Data.List(groupBy)
import Data.Map(Map,(!))
import qualified Data.Map
import Data.Set(Set,size,unions)
import qualified Data.Set

type Square = (Int,Set (Int,Int))

parseSquare :: String -> Square
parseSquare s = (length rows,Data.Set.fromList $ map snd $ filter ((== '#') . fst) $ concat points)
  where
    rows = filter (/= "/") $ groupBy inRow s
    inRow a b = a /= '/' && b /= '/'
    points = zipWith parseRow rows [0..]
    parseRow chars row = zipWith (parsePoint row) chars [0..]
    parsePoint row char col = (char,(col,row))

rotate :: Square -> Square
rotate (n,points) = (n,Data.Set.map rot points)
  where rot (x,y) = (n-1-y,x)

flipSquare :: Square -> Square
flipSquare (n,points) = (n,Data.Set.map fl points)
  where fl (x,y) = (n-1-x,y)

parseRules :: String -> Map Square Square
parseRules s = Data.Map.fromList $ p (words s)
  where
    p (srcSquare:"=>":destSquare:rest) =
        [(src,dest),
         (rotate src,dest),
         (rotate $ rotate src,dest),
         (rotate $ rotate $ rotate src,dest),
         (flipSquare src,dest),
         (rotate $ flipSquare src,dest),
         (rotate $ rotate $ flipSquare src,dest),
         (rotate $ rotate $ rotate $ flipSquare src,dest)] ++ p rest
      where src = parseSquare srcSquare
            dest = parseSquare destSquare
    p _ = []

translate :: (Int,Int) -> Square -> Set (Int,Int)
translate (x,y) (n,points) = Data.Set.map f points
  where f (col,row) = (col+x,row+y)

subsquare :: Square -> Int -> (Int,Int) -> Square
subsquare (n,points) newN (x,y) =
    (newN,Data.Set.filter f $ Data.Set.map xlate points)
  where xlate (col,row) = (col-x,row-y)
        f (col,row) = col >= 0 && col < newN && row >= 0 && row < newN

enhance :: Map Square Square -> Square -> Square
enhance rules sq@(n,_)
  | n `mod` 2 == 0 =
    (3*(n `div` 2),
     unions [translate (i*3,j*3) (rules!subsquare sq 2 (i*2,j*2))
             | i <- [0 .. n `div` 2 - 1], j <- [0 .. n `div` 2 - 1]])
  | otherwise =
    (4*(n `div` 3),
     unions [translate (i*4,j*4) (rules!subsquare sq 3 (i*3,j*3))
             | i <- [0 .. n `div` 3 - 1], j <- [0 .. n `div` 3 - 1]])

start :: Square
start = parseSquare ".#./..#/###"

testData :: String
testData = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"

test :: ()
test
  | size (snd . head $ drop 2 $ flip iterate start $ enhance $ parseRules testData) /= 12 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (size . snd . head . drop 5 . flip iterate start . enhance . parseRules) $ readFile "input/21.txt"

-- The part1 algorithm is too slow for part 2.
-- It gives counts for my input up to 13 iterations as:
-- 5 5 21 49 67 167 377 651 1521 3333 5895 13861 29921 53625

getCounts :: Int -> Map Square Square -> Int
getCounts iterations rules = getCounts3x3 rules iterations start

getCounts3x3 :: Map Square Square -> Int -> Square -> Int
getCounts3x3 rules iterations square
  | iterations <= 0 = size $ snd square
  | otherwise = getCounts4x4 rules (iterations-1) (rules!square)

getCounts4x4 :: Map Square Square -> Int -> Square -> Int
getCounts4x4 rules iterations square
  | iterations <= 0 = size $ snd square
  | otherwise = getCounts6x6 rules (iterations-1) (6,(unions
      [snd (rules!subsquare square 2 (0,0)),
       translate (3,0) (rules!subsquare square 2 (2,0)),
       translate (0,3) (rules!subsquare square 2 (0,2)),
       translate (3,3) (rules!subsquare square 2 (2,2))]))

getCounts6x6:: Map Square Square -> Int -> Square -> Int
getCounts6x6 rules iterations square
  | iterations <= 0 = size $ snd square
  | otherwise =
        getCounts3x3 rules (iterations-1) (rules!subsquare square 2 (0,0)) +
        getCounts3x3 rules (iterations-1) (rules!subsquare square 2 (2,0)) +
        getCounts3x3 rules (iterations-1) (rules!subsquare square 2 (4,0)) +
        getCounts3x3 rules (iterations-1) (rules!subsquare square 2 (0,2)) +
        getCounts3x3 rules (iterations-1) (rules!subsquare square 2 (2,2)) +
        getCounts3x3 rules (iterations-1) (rules!subsquare square 2 (4,2)) +
        getCounts3x3 rules (iterations-1) (rules!subsquare square 2 (0,4)) +
        getCounts3x3 rules (iterations-1) (rules!subsquare square 2 (2,4)) +
        getCounts3x3 rules (iterations-1) (rules!subsquare square 2 (4,4))

part2 :: IO Int
part2 = fmap (getCounts 18 . parseRules) $ readFile "input/21.txt"
