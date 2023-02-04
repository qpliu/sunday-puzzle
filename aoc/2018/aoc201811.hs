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
