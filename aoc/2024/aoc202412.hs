module AOC202412 where

import Data.Map(Map,alter,elems,empty,insert,keys,member,toList,(!))
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
    aocParse=parse2d,
    aocResult=result,
    aocParse2=parse2d,
    aocResult2=result2
    }

makeRegions :: Map (Int,Int) Char -> Map (Int,Int) (Int,Int)
makeRegions grid = foldr fill empty $ toList grid
  where
    fill (xy,ch) regions
      | member xy regions = regions
      | otherwise = foldr (fillWith xy ch xy) (insert xy xy regions)
                        $ neighbors xy
    fillWith region ch fromXY xy regions
      | Just ch /= Data.Map.lookup xy grid = regions
      | member xy regions = regions
      | otherwise = foldr (fillWith region ch xy) (insert xy region regions)
                        $ filter (/= fromXY) $ neighbors xy

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

toSizes :: Map (Int,Int) (Int,Int) -> Map (Int,Int) (Int,Int)
toSizes regions = foldr collect empty $ keys regions
  where
    collect xy@(x,y) sizes = alter (Just . maybe (1,perimeter) (\ (s,p) -> (s+1,p+perimeter))) region sizes
      where
        region = regions!xy
        perimeter = sum [1 | nxy <- neighbors xy,
                             Just region /= Data.Map.lookup nxy regions]

result = sum . map (uncurry (*)) . elems . toSizes . makeRegions

toSizes2 :: Map (Int,Int) (Int,Int) -> Map (Int,Int) (Int,Int)
toSizes2 regions = foldr collect empty $ keys regions
  where
    collect xy@(x,y) sizes =
        alter (Just . maybe (1,ends) (\ (s,e) -> (s+1,e+ends)))
              region sizes
      where
        region = regions!xy
        ends = sum [1 | dxdy@(dx,dy) <- [(1,0),(0,1),(-1,0),(0,-1)],
                        dydx <- [(dy,dx),(-dy,-dx)], isEnd dxdy dydx]
        inRegion (dx,dy) = Just region == Data.Map.lookup (x+dx,y+dy) regions
        isEnd dxdy@(dx,dy) dxdy2@(dx2,dy2) =
            not (inRegion dxdy) &&
                (inRegion dxdy2,inRegion (dx+dx2,dy+dy2)) /= (True,False)

result2 = (`div` 2) . sum . map (uncurry (*)) . elems . toSizes2 . makeRegions
