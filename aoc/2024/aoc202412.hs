module AOC202412 where

import Data.Array(Array,array,assocs,bounds,inRange,inRange,(!))
import Data.Map(Map,alter,elems,empty,insert,keys,member,toList)
import qualified Data.Map

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
    aocResult=result,
    aocParse2=parse2da,
    aocResult2=result2
    }

makeRegions :: Array (Int,Int) Char -> Array (Int,Int) (Int,Int)
makeRegions grid =
    array (bounds grid) $ toList $ foldr fill empty $ assocs grid
  where
    inBounds = inRange (bounds grid)
    fill (xy,ch) regions
      | member xy regions = regions
      | otherwise = foldr (fillWith xy ch xy) (insert xy xy regions)
                        $ filter inBounds $ neighbors xy
    fillWith region ch fromXY xy regions
      | grid!xy /= ch = regions
      | member xy regions = regions
      | otherwise = foldr (fillWith region ch xy) (insert xy region regions)
                        $ filter inBounds $ filter (/= fromXY) $ neighbors xy

getMetrics :: (Array (Int,Int) (Int,Int) -> (Int,Int) -> (Int,Int) -> Int)
                -> Array (Int,Int) (Int,Int) -> Map (Int,Int) (Int,Int)
getMetrics metric regions = foldr collect empty $ assocs regions
  where
    collect (xy@(x,y),region) = alter (Just . maybe (1,m) addMetric) region
      where
        m = metric regions xy region
        addMetric (oldCount,oldMetric) = (oldCount+1,oldMetric+m)

perimeter :: Array (Int,Int) (Int,Int) -> (Int,Int) -> (Int,Int) -> Int
perimeter regions xy region =
    length [() | n <- neighbors xy, not (inBounds n && regions!n == region)]
  where
    inBounds = inRange (bounds regions)

-- Counts south ends of fences to the east, north ends of fences to the west,
-- east ends of fences to the south, and west ends of fences to the north.
sides :: Array (Int,Int) (Int,Int) -> (Int,Int) -> (Int,Int) -> Int
sides regions (x,y) region =
    length [() | dxy <- [(1,0),(-1,0),(0,1),(0,-1)], isEnd dxy]
  where
    inRegion (dx,dy) = inRange (bounds regions) (x+dx,y+dy)
                            && region == regions!(x+dx,y+dy)
    isEnd (dx,dy) = not (inRegion (dx,dy))
        && (inRegion (dy,dx),inRegion (dx+dy,dy+dx)) /= (True,False)

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

result :: Array (Int,Int) Char -> Int
result = sum . map (uncurry (*)) . elems . getMetrics perimeter . makeRegions

result2 :: Array (Int,Int) Char -> Int
result2 = sum . map (uncurry (*)) . elems . getMetrics sides . makeRegions
