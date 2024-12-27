module AOC202412 where

import Data.Array(Array,assocs,bounds,inRange,(!))
import Data.Set(Set,empty,insert,member)

import AOC

aoc = AOC {
    day="12",
    aocTests=[
        AOCTest {
            testData=unlines [
                "AAAA",
                "BBCD",
                "BBCC",
                "EEEC"
                ],
            testResult=Just "140",
            testResult2=Just "80"
            },
        AOCTest {
            testData=unlines [
                "OOOOO",
                "OXOXO",
                "OOOOO",
                "OXOXO",
                "OOOOO"
                ],
            testResult=Just "772",
            testResult2=Just "436"
            },
        AOCTest {
            testData=unlines [
                "AAAAAA",
                "AAABBA",
                "AAABBA",
                "ABBAAA",
                "ABBAAA",
                "AAAAAA"
                ],
            testResult=Nothing,
            testResult2=Just "368"
            },
        AOCTest {
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
            testResult=Just "1930",
            testResult2=Just "1206"
            }
        ],
    aocCode=Code {
        codeParse=parse2da,
        codeParse2=parse2da,
        codeTest=result perimeter,
        codeTest2=result sides,
        codeResult=result perimeter,
        codeResult2=result sides
        }
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
