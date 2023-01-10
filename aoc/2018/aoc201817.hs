{-
--- Day 17: Reservoir Research ---

You arrive in the year 18. If it weren't for the coat you got in 1018, you
would be very cold: the North Pole base hasn't even been constructed.

Rather, it hasn't been constructed yet. The Elves are making a little progress,
but there's not a lot of liquid water in this climate, so they're getting very
dehydrated. Maybe there's more underground?

You scan a two-dimensional vertical slice of the ground nearby and discover
that it is mostly sand with veins of clay. The scan only provides data with a
granularity of square meters, but it should be good enough to determine how
much water is trapped there. In the scan, x represents the distance to the
right, and y represents the distance down. There is also a spring of water near
the surface at x=500, y=0. The scan identifies which square meters are clay
(your puzzle input).

For example, suppose your scan shows the following veins of clay:

| x=495, y=2..7
| y=7, x=495..501
| x=501, y=3..7
| x=498, y=2..4
| x=506, y=1..2
| x=498, y=10..13
| x=504, y=10..13
| y=13, x=498..504

Rendering clay as #, sand as ., and the water spring as +, and with x
increasing to the right and y increasing downward, this becomes:

|    44444455555555
|    99999900000000
|    45678901234567
|  0 ......+.......
|  1 ............#.
|  2 .#..#.......#.
|  3 .#..#..#......
|  4 .#..#..#......
|  5 .#.....#......
|  6 .#.....#......
|  7 .#######......
|  8 ..............
|  9 ..............
| 10 ....#.....#...
| 11 ....#.....#...
| 12 ....#.....#...
| 13 ....#######...

The spring of water will produce water forever. Water can move through sand,
but is blocked by clay. Water always moves down when possible, and spreads to
the left and right otherwise, filling space that has clay on both sides and
falling out otherwise.

For example, if five squares of water are created, they will flow downward
until they reach the clay and settle there. Water that has come to rest is
shown here as ~, while sand through which water has passed (but which is now
dry again) is shown as |:

| ......+.......
| ......|.....#.
| .#..#.|.....#.
| .#..#.|#......
| .#..#.|#......
| .#....|#......
| .#~~~~~#......
| .#######......
| ..............
| ..............
| ....#.....#...
| ....#.....#...
| ....#.....#...
| ....#######...

Two squares of water can't occupy the same location. If another five squares of
water are created, they will settle on the first five, filling the clay
reservoir a little more:

| ......+.......
| ......|.....#.
| .#..#.|.....#.
| .#..#.|#......
| .#..#.|#......
| .#~~~~~#......
| .#~~~~~#......
| .#######......
| ..............
| ..............
| ....#.....#...
| ....#.....#...
| ....#.....#...
| ....#######...

Water pressure does not apply in this scenario. If another four squares of
water are created, they will stay on the right side of the barrier, and no
water will reach the left side:

| ......+.......
| ......|.....#.
| .#..#.|.....#.
| .#..#~~#......
| .#..#~~#......
| .#~~~~~#......
| .#~~~~~#......
| .#######......
| ..............
| ..............
| ....#.....#...
| ....#.....#...
| ....#.....#...
| ....#######...

At this point, the top reservoir overflows. While water can reach the tiles
above the surface of the water, it cannot settle there, and so the next five
squares of water settle like this:

| ......+.......
| ......|.....#.
| .#..#||||...#.
| .#..#~~#|.....
| .#..#~~#|.....
| .#~~~~~#|.....
| .#~~~~~#|.....
| .#######|.....
| ........|.....
| ........|.....
| ....#...|.#...
| ....#...|.#...
| ....#~~~~~#...
| ....#######...

Note especially the leftmost |: the new squares of water can reach this tile,
but cannot stop there. Instead, eventually, they all fall to the right and
settle in the reservoir below.

After 10 more squares of water, the bottom reservoir is also full:

| ......+.......
| ......|.....#.
| .#..#||||...#.
| .#..#~~#|.....
| .#..#~~#|.....
| .#~~~~~#|.....
| .#~~~~~#|.....
| .#######|.....
| ........|.....
| ........|.....
| ....#~~~~~#...
| ....#~~~~~#...
| ....#~~~~~#...
| ....#######...

Finally, while there is nowhere left for the water to settle, it can reach a
few more tiles before overflowing beyond the bottom of the scanned data:

| ......+.......    (line not counted: above minimum y value)
| ......|.....#.
| .#..#||||...#.
| .#..#~~#|.....
| .#..#~~#|.....
| .#~~~~~#|.....
| .#~~~~~#|.....
| .#######|.....
| ........|.....
| ...|||||||||..
| ...|#~~~~~#|..
| ...|#~~~~~#|..
| ...|#~~~~~#|..
| ...|#######|..
| ...|.......|..    (line not counted: below maximum y value)
| ...|.......|..    (line not counted: below maximum y value)
| ...|.......|..    (line not counted: below maximum y value)

How many tiles can be reached by the water? To prevent counting forever, ignore
tiles with a y coordinate smaller than the smallest y coordinate in your scan
data or larger than the largest one. Any x coordinate is valid. In this
example, the lowest y coordinate given is 1, and the highest is 13, causing the
water spring (in row 0) and the water falling off the bottom of the render (in
rows 14 through infinity) to be ignored.

So, in the example above, counting both water at rest (~) and other sand tiles
the water can hypothetically reach (|), the total number of tiles the water can
reach is 57.

How many tiles can the water reach within the range of y values in your scan?
-}

import Data.Char(isDigit)
import Data.Map(Map,elems,findWithDefault,fromList,insert)

type Ground = ((Int,Int),Map (Int,Int) Char)

parse :: String -> Ground
parse s = ((minimum (map snd clay),maximum (map snd clay)),fromList (zip clay (repeat '#')))
  where
    clay = concatMap parseVein (lines s)
    num = read . filter isDigit
    parseVein s
      | head s == 'x' = [(num n1,y) | y <- [num n2 .. num n3]]
      | otherwise = [(x,num n1) | x <- [num n2 .. num n3]]
      where
        (n1,s2) = span (/= ',') s
        (n2,n3) = span (/= '.') s2

l :: (Int,Int) -> (Int,Int)
l (x,y) = (x-1,y)

r :: (Int,Int) -> (Int,Int)
r (x,y) = (x+1,y)

u :: (Int,Int) -> (Int,Int)
u (x,y) = (x,y-1)

d :: (Int,Int) -> (Int,Int)
d (x,y) = (x,y+1)

look :: Ground -> (Int,Int) -> Char
look ((ymin,ymax),grid) (x,y)
  | y < ymin || y > ymax = '.'
  | otherwise = findWithDefault '.' (x,y) grid

set :: Ground -> (Int,Int) -> Char -> Ground
set ground@((ymin,ymax),grid) (x,y) char
  | y < ymin || y > ymax = ground
  | otherwise = ((ymin,ymax),insert (x,y) char grid)

flow :: (Int,Int) -> Ground -> Either Ground Ground
flow xy@(x,y) ground@((ymin,ymax),grid)
  | y < ymin = flow (x,ymin) ground
  | y > ymax = Left ground
  | look ground xy == '.' && y == ymax = Right (set ground xy '|')
  | look ground xy == '.' = flow xy (set ground xy '|')
  | look ground xy /= '|' = error (show (xy,look ground xy))
  | look ground (d xy) `elem` ".|" = flow (d xy) ground
  | leftContained && rightContained = Right (fill (fill ground l xy '~') r xy '~')
  | look ground (l xy) == '.' = Right (fill ground l xy '|')
  | look ground (r xy) == '.' = Right (fill ground r xy '|')
  | otherwise = maybe (maybe (Left ground) Right flowRight) Right flowLeft
  where
    leftContained = contained l xy
    rightContained = contained r xy
    flowLeft = flowSide l xy
    flowRight = flowSide r xy

    fill ground dir xy char
      | look ground xy == '#' = ground
      | look ground (d xy) `elem` ".|" = set ground xy char
      | otherwise = fill (set ground xy char) dir (dir xy) char

    contained dir xy
      | look ground xy == '#' = True
      | look ground (d xy) `elem` ".|" = False
      | otherwise = contained dir (dir xy)

    flowSide dir xy
      | look ground xy == '#' = Nothing
      | look ground (d xy) `elem` ".|" = either (const Nothing) Just (flow (d xy) ground)
      | otherwise = flowSide dir (dir xy)

spring :: (Int,Int) -> Ground -> Ground
spring source ground = either id (spring source) (flow source ground)

countWater :: Ground -> Int
countWater (_,grid) = length $ filter (`elem` "~|") $ elems grid

testData :: String
testData = "x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504"

test :: ()
test
  | countWater (spring (500,0) $ parse testData) /= 57 = error "a"
  | otherwise = ()

-- This is too slow.  The Go code is slow too, but eventually gets the answer.
part1 :: IO Int
part1 = fmap (countWater . spring (500,0) . parse) $ readFile "input/17.txt"
