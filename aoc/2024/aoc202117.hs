module AOC202117 where

import Data.Map(Map,alter,findWithDefault,keys)
import qualified Data.Map
import Data.Set(Set,empty,insert,singleton,size,unions)

import AOC

aoc = AOC {
    day="../../2021/input/17",
    aocTests=[
        AOCTest {
            testData="target area: x=20..30, y=-10..-5",
            testResult=Just "45",
            testResult2=Just "112"
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=parseInts,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

-- Let x1,x2 and y1,y2 be the target area.

-- Given y1, vy = -y1-1 is the upper bound, which hits the bottom of the y
-- target area at t = 2*vy+1, with a maximum height of (vy*(vy+1))`div`2.

-- Given x1, the minimum vx is (-1+sqrt(8*x1+1))/2, hitting the left edge
-- of the x target area at t = vx, and remains in the x target area for all
-- later times.

-- For the test data and my input, the x target area is reached with the
-- minimum vx before the trajectory with the max vy passes through the y
-- target area.
result [x1,x2,y1,y2]
  | vx <= 2*vy+1 = (vy*(vy+1)) `div` 2
  | otherwise = error (show "more work needed")
  where
    vy = -y1-1
    vx = ceiling ((-1+sqrt(8*fromIntegral x1+1))/2)

-- Given y1, the range for vy is from y1 to -y1-1.  Outside that range,
-- the y target area will not be hit at any t when t is an integer.
-- y = vy*t - (t*(t-1))`div`2
timesInYTargetArea :: [Int] -> Map Int (Set Int)
timesInYTargetArea [_,_,y1,y2] =
    foldr collect Data.Map.empty $ trajectories
  where
    collect (vy,t) table =
        alter (Just . maybe (singleton vy) (insert vy)) t table
    ry1 = fromIntegral y1
    ry2 = fromIntegral y2
    trajectories =
        [(vy,t) | vy <- [y1 .. -y1-1],
                  rvy <- [fromIntegral vy],
                  t1 <- [ceiling (rvy+1/2*sqrt(4*rvy^2+4*rvy-8*ry1+1)+1/2)],
                  t2 <- [floor (rvy+1/2*sqrt(4*rvy^2+4*rvy-8*ry2+1)+1/2)],
                  t <- [t2..t1],
                  y <- [vy*t-(t*(t-1))`div`2],
                  y1 <= y && y <= y2]

-- x = vx*t - (t*(t-1))`div`2 for t < vx
-- x = vx^2 - (vx^2+vx)`div`2 = (vx^2-vx)`div`2 for t >= vx
waysInTargetArea :: [Int] -> Map Int (Set Int) -> Int
waysInTargetArea [x1,x2,_,_] targetTimes =
    sum [ways vx | vx <- [1 .. x2+1]]
  where
    tmax = maximum $ keys targetTimes
    ways vx = size $ unions [findWithDefault empty t targetTimes | t <- times]
      where
        rvx = fromIntegral vx
        times = [t | t <- [0..tmax],
                     tt <- [min t vx],
                     x <- [vx*tt - (tt*(tt-1))`div`2],
                     x1 <= x && x <= x2]

result2 targetArea =
    waysInTargetArea targetArea $ timesInYTargetArea targetArea
