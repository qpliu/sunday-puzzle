import Data.List(nub,sort)

parse :: String -> [((Int,Int),(Int,Int))]
parse = map (p . words) . lines
  where
    p ["Sensor","at",sx,sy,"closest","beacon","is","at",bx,by] =
        ((pnum sx,pnum sy),(pnum bx,pnum by))
    pnum = read . filter (not . (`elem` ",:")) . drop 2

addRange :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
addRange [] range = [range]
addRange ranges@((lo1,hi1):rest) (lo,hi)
  | hi+1 < lo1 = (lo,hi):ranges
  | hi <= hi1 = (min lo lo1,hi1):rest
  | lo <= hi1+1 = addRange rest (min lo lo1,hi)
  | otherwise = (lo1,hi1):addRange rest (lo,hi)

rangesSize :: [(Int,Int)] -> Int
rangesSize = sum . map rangeSize
  where rangeSize (lo,hi) = hi-lo+1

getBeaconlessRanges :: Int -> [((Int,Int),(Int,Int))] -> [(Int,Int)]
getBeaconlessRanges row sensors = foldr (addBeaconlessRange row) [] sensors

addBeaconlessRange :: Int -> ((Int,Int),(Int,Int)) -> [(Int,Int)] -> [(Int,Int)]
addBeaconlessRange row sensor ranges =
    maybe ranges (addRange ranges) (getBeaconlessRange row sensor)

getBeaconlessRange :: Int -> ((Int,Int),(Int,Int)) -> Maybe (Int,Int)
getBeaconlessRange row (sxy@(sx,sy),bxy@(bx,by))
  | dx < 0 = Nothing
  | dx == 0 && row == by = Nothing
  | row == by && sx-dx == bx = Just (sx-dx+1,sx+dx)
  | row == by && sx+dx == bx = Just (sx-dx,sx+dx-1)
  | otherwise = Just (sx-dx,sx+dx)
  where
    dx = dist sxy bxy - abs (sy-row)

dist :: (Int,Int) -> (Int,Int) -> Int
dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

testData :: String
testData = unlines [
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
    "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
    "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
    "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
    "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
    "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
    "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
    "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
    "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
    "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
    "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
    "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
    ]

test :: ()
test
  | (rangesSize . getBeaconlessRanges 10 . parse) testData /= 26 = error "a"
  | (getBeaconlessRanges 9 . parse) testData /= [(-1,23)] = error "b"
  | (getBeaconlessRanges 10 . parse) testData /= [(-2,1),(3,24)] = error "c"
  | (getBeaconlessRanges 11 . parse) testData /= [(-3,13),(15,25)] = error "d"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (rangesSize . getBeaconlessRanges 2000000 . parse) $ readFile "input/15.txt"

-- For part 2,
-- sort the x coordinates of the sensors, then consider the each
-- range of x between a sensor and the sensor with the least greater x.
-- Also, consider the ranges 0 to the smallest x and the greatest x to
-- 4000000, unless the smallest x is less than 0 or the greatest x is
-- more than 4000000.

-- For each x range, divide the y range similarly.

-- This gives a rectangular region.  All the sensors can be classified
-- as being NW of the NW corner, NE of the NE corner, SW of the SW
-- corner or SE of the SE corner.

-- Then for each of the corners, there is one sensor that has the
-- greatest range beyond the corner.

-- If, for opposite corners, the sum of range their sensor beyond the
-- corner is greater than or equal to the distance between the corners,
-- this region has no uncovered positions.

toCoverage :: ((Int,Int),(Int,Int)) -> ((Int,Int),Int)
toCoverage ((sx,sy),(bx,by)) = ((sx,sy),abs (sx-bx) + abs (sy-by))

ranges :: Int -> Int -> [Int] -> [(Int,Int)]
ranges min max [] = error "ranges"
ranges min max [v]
  | v == min = [(min,max)]
  | v < max = [(min,v),(v,max)]
  | otherwise = [(min,max)]
ranges min max (v:vs)
  | v == min = ranges min max vs
  | v < max = (min,v) : ranges v max vs
  | otherwise = [(min,max)]

xranges :: (Int,Int) -> [((Int,Int),Int)] -> [(Int,Int)]
xranges (xmin,xmax) sensors = ranges xmin xmax allXs
  where
    allXs = sort $ map (fst . fst) sensors

yranges :: (Int,Int) -> [((Int,Int),Int)] -> [(Int,Int)]
yranges (ymin,ymax) sensors = ranges ymin ymax allYs
  where
    allYs = sort $ map (snd . fst) sensors

regions :: Int -> [((Int,Int),Int)] -> [((Int,Int),(Int,Int))]
regions max sensors = [(xr,yr) | xr <- xranges (0,max) sensors, yr <- yranges (0,max) sensors]

regionUncovered :: [((Int,Int),Int)] -> ((Int,Int),(Int,Int)) -> [(Int,Int)]
regionUncovered sensors ((x1,x2),(y1,y2))
    -- If either pair of opposite corners covers the whole region,
    -- the region is completely covered.
    | nwCover + seCover + 1 >= regionSize = []
    | neCover + swCover + 1 >= regionSize = []
    -- Each pair of opposite corners leaves an uncovered diagonal stripe.
    -- If the stripes intersect within the region, the region is not
    -- comletely covered.
    -- The reasoning has been too difficult.  Just walk along the edge
    -- outside the range of the NW corner, and then return the points that
    -- are not covered by any of the other 3 corners.
    | otherwise = [xy | i <- [0..x2-x1], xy@(x,y) <- [(x1+i,y1+nwCover+1-i)],
                    y >= y1 && y <= y2,
                    dist xy (x2,y1) > neCover,
                    dist xy (x1,y2) > swCover,
                    dist xy (x2,y2) > seCover]
  where
    regionSize = dist (x1,y1) (x2,y2)
    nw = filter (\ ((x,y),_) -> x <= x1 && y <= y1) sensors
    ne = filter (\ ((x,y),_) -> x >= x2 && y <= y1) sensors
    sw = filter (\ ((x,y),_) -> x <= x1 && y >= y2) sensors
    se = filter (\ ((x,y),_) -> x >= x2 && y >= y2) sensors
    greatestCoverage cornerXY sensors = maximum (-1:[sensorRange - dist cornerXY sensorXY | (sensorXY,sensorRange) <- sensors])
    nwCover = greatestCoverage (x1,y1) nw
    neCover = greatestCoverage (x2,y1) ne
    swCover = greatestCoverage (x1,y2) sw
    seCover = greatestCoverage (x2,y2) se

tuningFreq :: (Int,Int) -> Int
tuningFreq (x,y) = x*4000000+y

run2 :: Int -> String -> [Int]
run2 max input =
    map tuningFreq $ nub $ concatMap (regionUncovered sensors) (regions max sensors)
  where
    sensors = map toCoverage $ parse input

test2 :: ()
test2
  | run2 20 testData /= [56000011] = error "a"
  | otherwise = ()

part2 :: IO [Int]
part2 = fmap (run2 4000000) $ readFile "input/15.txt"
