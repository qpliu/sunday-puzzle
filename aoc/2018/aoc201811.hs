{-
--- Day 11: Chronal Charge ---

You watch the Elves and their sleigh fade into the distance as they head toward
the North Pole.

Actually, you're the one fading. The falling sensation returns.

The low fuel warning light is illuminated on your wrist-mounted device. Tapping
it once causes it to project a hologram of the situation: a 300x300 grid of
fuel cells and their current power levels, some negative. You're not sure what 
negative power means in the context of time travel, but it can't be good.

Each fuel cell has a coordinate ranging from 1 to 300 in both the X
(horizontal) and Y (vertical) direction. In X,Y notation, the top-left cell is
1,1, and the top-right cell is 300,1.

The interface lets you select any 3x3 square of fuel cells. To increase your
chances of getting to your destination, you decide to choose the 3x3 square
with the largest total power.

The power level in a given fuel cell can be found through the following
process:

 - Find the fuel cell's rack ID, which is its X coordinate plus 10.
 - Begin with a power level of the rack ID times the Y coordinate.
 - Increase the power level by the value of the grid serial number (your puzzle
   input).
 - Set the power level to itself multiplied by the rack ID.
 - Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers
   with no hundreds digit become 0).
 - Subtract 5 from the power level.

For example, to find the power level of the fuel cell at 3,5 in a grid with
serial number 8:

 - The rack ID is 3 + 10 = 13.
 - The power level starts at 13 * 5 = 65.
 - Adding the serial number produces 65 + 8 = 73.
 - Multiplying by the rack ID produces 73 * 13 = 949.
 - The hundreds digit of 949 is 9.
 - Subtracting 5 produces 9 - 5 = 4.

So, the power level of this fuel cell is 4.

Here are some more example power levels:

 - Fuel cell at  122,79, grid serial number 57: power level -5.
 - Fuel cell at 217,196, grid serial number 39: power level  0.
 - Fuel cell at 101,153, grid serial number 71: power level  4.

Your goal is to find the 3x3 square which has the largest total power. The
square must be entirely within the 300x300 grid. Identify this square using the
X,Y coordinate of its top-left fuel cell. For example:

For grid serial number 18, the largest total 3x3 square has a top-left corner
of 33,45 (with a total power of 29); these fuel cells appear in the middle of
this 5x5 region:

| -2  -4   4   4   4
| -4   4   4   4  -5
|  4   3   3   4  -4
|  1   1   2   4  -3
| -1   0   2  -5  -2

For grid serial number 42, the largest 3x3 square's top-left is 21,61 (with a
total power of 30); they are in the middle of this region:

| -3   4   2   2   2
| -4   4   3   3   4
| -5   3   3   4  -4
|  4   3   3   4  -3
|  3   3   3  -5  -1

What is the X,Y coordinate of the top-left fuel cell of the 3x3 square with the
largest total power?
-}

import Debug.Trace(traceShow)

import Data.Array(Array,array,assocs,bounds,(!))
import Data.Tuple(swap)

powerLevel :: Int -> (Int,Int) -> Int
powerLevel serialNumber (x,y) = ((((x+10)*y+serialNumber)*(x+10)) `div` 100) `mod` 10 - 5

find3x3 :: Int -> (Int,(Int,Int))
find3x3 serialNumber = maximum [(powerLevel serialNumber (x,y)+powerLevel serialNumber (x+1,y)+powerLevel serialNumber (x+2,y)+powerLevel serialNumber (x,y+1)+powerLevel serialNumber (x+1,y+1)+powerLevel serialNumber (x+2,y+1)+powerLevel serialNumber (x,y+2)+powerLevel serialNumber (x+1,y+2)+powerLevel serialNumber (x+2,y+2),(x,y)) | x <- [1..298], y <- [1..298]]

test :: ()
test
  | powerLevel 57 (122,79) /= -5 = error "a"
  | powerLevel 39 (217,196) /= 0 = error "b"
  | powerLevel 71 (101,153) /= 4 = error "c"
  | find3x3 18 /= (29,(33,45)) = error "d"
  | find3x3 42 /= (30,(21,61)) = error "e"
  | otherwise = ()

part1 :: Int -> (Int,Int)
part1 serialNumber = snd $ find3x3 serialNumber

makeGrid :: Int -> Int -> Array (Int,Int) Int
makeGrid size serialNumber = array ((1,1),(size,size)) [((x,y),powerLevel serialNumber (x,y)) | x <- [1..size], y <- [1..size]]

findNxN :: Array (Int,Int) Int -> Int -> (Int,(Int,Int,Int))
findNxN grid n = maximum [getNxN x y | x <- [1..size+1-n], y <- [1..size+1-n]]
  where
    (_,(size,_)) = bounds grid
    getNxN x y = (sum [grid!(x+i,y+j) | i <- [0..n-1], j <- [0..n-1]],(x,y,n))

-- This is very slow
findN :: Array (Int,Int) Int -> (Int,(Int,Int,Int))
findN grid = maximum $ map (findNxN grid) [1..size]
  where
    (_,(size,_)) = bounds grid

-- Probably need to turn the grid into 150x150, 150x149, 149x150, 149x149
-- grids for sizes with a factor of 2.
-- Turn the grid into 100x100, 2 100x99, 2 99x100, 4 99x99 grids for sizes
-- with a factor of 3.
-- Turn the grid 150x150 into 75x75, 75x74, 74x75, 74x74 and 149x149 into
-- 4 74x74 for sizes with a factor of 4.
-- Turn the 300x300 into 60x60, 4 60x59, 4 59x60 and 16 59x59 for sizes
-- with a factor of 5.
-- And so on...

-- This is still unsatisfactorily slow, especially when considering prime
-- sizes.
--
-- It looks like the maximums for sizes greater than around 25 are negative
-- so only consider smaller sizes.

initialGrid :: Int -> Int -> [(Int,[((Int,Int),Array (Int,Int) Int)])]
initialGrid size serialNumber = [(1,[((1,1),grid)])]
  where
    grid = array ((0,0),(size-1,size-1)) [((x,y),powerLevel serialNumber (x+1,y+1)) | x <- [0..size-1], y <- [0..size-1]]

addGrid :: [(Int,[((Int,Int),Array (Int,Int) Int)])] -> [(Int,[((Int,Int),Array (Int,Int) Int)])]
addGrid prevGrids@((prevn,_):_) = newGrid prevGrids:prevGrids
  where
    n = prevn + 1
    newGrid ((m,grids):rest)
      | n `mod` m /= 0 = newGrid rest
      | otherwise = (n,concatMap quilts grids)
      where
        subSize = n `div` m
        quilts ((xoffset,yoffset),grid) = [((xoffset+i*m,yoffset+j*m),quilt i j) | i <- [0..subSize-1], j <- [0..subSize-1], nonzero i j]
          where
            (_,(px,py)) = bounds grid
            nonzero i j = px-i >= subSize && py-j >= subSize
            quilt i j = --traceShow ((n,m),(px,py),(i,j),(px2,py2),subSize)
                array ((0,0),(px2,py2)) [((xx,yy),sum [grid!(i+xx*subSize+ii,j+yy*subSize+jj) | ii <- [0..subSize-1], jj <- [0..subSize-1]]) | xx <- [0..px2], yy <- [0..py2]]
              where
                (px2,py2) = ((px+1-i)`div`subSize-1,(py+1-j)`div`subSize-1)

maxOfSize :: (Int,[((Int,Int),Array (Int,Int) Int)]) -> (Int,(Int,Int,Int))
maxOfSize (size,grids) = (power,(x*size+xoffset,y*size+yoffset,size))
  where
    ((power,(x,y)),(xoffset,yoffset)) = maximum $ map (swap . fmap (maximum . map swap . assocs)) grids

maxOfAllSizes :: [(Int,[((Int,Int),Array (Int,Int) Int)])] -> (Int,(Int,Int,Int))
maxOfAllSizes = maximum . map maxOfSize

test2 :: ()
test2
  | maxOfAllSizes (head $ drop 25 $ iterate addGrid $ initialGrid 300 18) /= (113,(96,269,18)) = error "a"
  | maxOfAllSizes (head $ drop 25 $ iterate addGrid $ initialGrid 300 42) /= (119,(232,251,12)) = error "b"
  | otherwise = ()

part2 :: Int -> (Int,Int,Int)
part2 serialNumber = snd $ maxOfAllSizes (head $ drop 25 $ iterate addGrid $ initialGrid 300 serialNumber)
