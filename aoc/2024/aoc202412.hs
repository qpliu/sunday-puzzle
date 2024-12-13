module AOC202412 where

import Data.Array(Array,assocs,bounds,inRange,(!))
import Data.Set(Set,empty,insert,member)

import AOC

aoc = AOC {
    day="12",
    testData=unlines [
    "RRRRIICCFF",
    "RRRRIICCCF",
    "VVRRRCCFFF",
    "VVRCCCJFFF",
    "VVVVCJJCFE",
    "VVIVCCJJEE",
    "VVIIICJJEE",
    "MIIIIIJJEE",
    "MIIISIJEEE",
    "MMMISSJEEE"
    ],
    testResult="1930",
    testData2="",
    testResult2="1206",
    aocParse=parse2da,
    aocResult=result perimeter,
    aocParse2=parse2da,
    aocResult2=result sides
    }

measureRegions :: (Array (Int,Int) Char -> Char -> (Int,Int) -> Int)
               -> Array (Int,Int) Char -> [(Int,Int)]
measureRegions metric grid = snd $ foldr measure (empty,[]) $ assocs grid
  where
    inBounds = inRange (bounds grid)
    measure (xy,ch) (seen,results)
      | member xy seen = (seen,results)
      | otherwise = fmap (:results)
                      $ foldr (fill ch) (insert xy seen,(1,metric grid ch xy))
                      $ filter inBounds $ neighbors xy
    fill ch xy (seen,(count,m))
      | grid!xy /= ch || member xy seen = (seen,(count,m))
      | otherwise =
          foldr (fill ch) (insert xy seen,(1+count,m+metric grid ch xy))
              $ filter inBounds $ neighbors xy

perimeter :: Array (Int,Int) Char -> Char -> (Int,Int) -> Int
perimeter regions region xy =
    length [() | n <- neighbors xy, not (inBounds n && regions!n == region)]
  where
    inBounds = inRange (bounds regions)

-- Counts south ends of fences to the east, north ends of fences to the west,
-- east ends of fences to the south, and west ends of fences to the north.
sides :: Array (Int,Int) Char -> Char -> (Int,Int) -> Int
sides regions region (x,y) =
    length [() | dxy <- [(1,0),(-1,0),(0,1),(0,-1)], isEnd dxy]
  where
    inRegion (dx,dy) = inRange (bounds regions) (x+dx,y+dy)
                            && region == regions!(x+dx,y+dy)
    isEnd (dx,dy) = not (inRegion (dx,dy))
        && (inRegion (dy,dx),inRegion (dx+dy,dy+dx)) /= (True,False)

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

result metric = sum . map (uncurry (*)) . measureRegions metric
