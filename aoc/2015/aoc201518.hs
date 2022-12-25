{-
--- Day 18: Like a GIF For Your Yard ---

After the million lights incident, the fire code has gotten stricter: now, at
most ten thousand lights are allowed. You arrange them in a 100x100 grid.

Never one to let you down, Santa again mails you instructions on the ideal
lighting configuration. With so few lights, he says, you'll have to resort to
animation.

Start by setting your lights to the included initial configuration (your puzzle
input). A # means "on", and a . means "off".

Then, animate your grid in steps, where each step decides the next
configuration based on the current one. Each light's next state (either on or
off) depends on its current state and the current states of the eight lights
adjacent to it (including diagonals). Lights on the edge of the grid might have
fewer than eight neighbors; the missing ones always count as "off".

For example, in a simplified 6x6 grid, the light marked A has the neighbors
numbered 1 through 8, and the light marked B, which is on an edge, only has the
neighbors marked 1 through 5:

1B5...
234...
......
..123.
..8A4.
..765.
The state a light should have next is based on its current state (on or off)
plus the number of neighbors that are on:

 - A light which is on stays on when 2 or 3 neighbors are on, and turns off
   otherwise.
 - A light which is off turns on if exactly 3 neighbors are on, and stays off
   otherwise.
 - All of the lights update simultaneously; they all consider the same current
   state before moving to the next.

Here's a few steps from an example configuration of another 6x6 grid:

Initial state:
.#.#.#
...##.
#....#
..#...
#.#..#
####..

After 1 step:
..##..
..##.#
...##.
......
#.....
#.##..

After 2 steps:
..###.
......
..###.
......
.#....
.#....

After 3 steps:
...#..
......
...#..
..##..
......
......

After 4 steps:
......
......
..##..
..##..
......
......
After 4 steps, this example has four lights on.

In your grid of 100x100 lights, given your initial configuration, how many lights are on after 100 steps?
-}
import Data.Set(Set,fromList,insert,member,size)

parse :: String -> ((Int,Int),Set (Int,Int))
parse s = ((maximum (map length rows),length rows),fromList (concatMap (parseRow) (zip [1..] rows)))
  where
    rows = lines s
    parseRow (row,gridLine) = map (((,) row) . fst) $ filter ((== '#') . snd) $ zip [1..] gridLine

-- This code is kind of slow
step :: ((Int,Int),Set (Int,Int)) -> ((Int,Int),Set (Int,Int))
step ((rows,columns),grid) = ((rows,columns),fromList [(r,c) | r <- [1..rows], c <- [1..columns], on r c])
  where
    on r c
      | (r,c) `member` grid = neighborhood r c `elem` [3,4]
      | otherwise = neighborhood r c == 3
    neighborhood r c = length [() | dr <- [-1,0,1], dc <- [-1,0,1], (r+dr,c+dc) `member` grid]

test 
  | step s0 /= s1 = error "a"
  | step s1 /= s2 = error "b"
  | step s2 /= s3 = error "c"
  | step s3 /= s4 = error "d"
  | otherwise = ()
  where
    s0 = parse ".#.#.#\n...##.\n#....#\n..#...\n#.#..#\n####.."
    s1 = parse "..##..\n..##.#\n...##.\n......\n#.....\n#.##.."
    s2 = parse "..###.\n......\n..###.\n......\n.#....\n.#...."
    s3 = parse "...#..\n......\n...#..\n..##..\n......\n......"
    s4 = parse "......\n......\n..##..\n..##..\n......\n......"

part1 :: IO Int
part1 = fmap (size . snd . head . drop 100 . iterate step . parse) (readFile "input/18.txt")

addCorners :: ((Int,Int),Set (Int,Int)) -> ((Int,Int),Set (Int,Int))
addCorners ((rows,columns),grid) = ((rows,columns),insert (1,1) $ insert (1,columns) $ insert (rows,1) $ insert (rows,columns) grid)

part2 :: IO Int
part2 = fmap (size . snd . head . drop 100 . iterate (addCorners . step) . addCorners . parse) (readFile "input/18.txt")
