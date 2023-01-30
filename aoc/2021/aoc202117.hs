{-
--- Day 17: Trick Shot ---

You finally decode the Elves' message. HI, the message says. You continue
searching for the sleigh keys.

Ahead of you is what appears to be a large ocean trench. Could the keys have
fallen into it? You'd better send a probe to investigate.

The probe launcher on your submarine can fire the probe with any integer
velocity in the x (forward) and y (upward, or downward if negative) directions.
For example, an initial x,y velocity like 0,10 would fire the probe straight
up, while an initial velocity like 10,-1 would fire the probe forward at a
slight downward angle.

The probe's x,y position starts at 0,0. Then, it will follow some trajectory by
moving in steps. On each step, these changes occur in the following order:

 - The probe's x position increases by its x velocity.
 - The probe's y position increases by its y velocity.
 - Due to drag, the probe's x velocity changes by 1 toward the value 0; that
   is, it decreases by 1 if it is greater than 0, increases by 1 if it is less
   than 0, or does not change if it is already 0.
 - Due to gravity, the probe's y velocity decreases by 1.

For the probe to successfully make it into the trench, the probe must be on
some trajectory that causes it to be within a target area after any step. The
submarine computer has already calculated this target area (your puzzle input).
For example:

| target area: x=20..30, y=-10..-5

This target area means that you need to find initial x,y velocity values such
that after any step, the probe's x position is at least 20 and at most 30, and
the probe's y position is at least -10 and at most -5.

Given this target area, one initial velocity that causes the probe to be within
the target area after any step is 7,2:

| .............#....#............
| .......#..............#........
| ...............................
| S........................#.....
| ...............................
| ...............................
| ...........................#...
| ...............................
| ....................TTTTTTTTTTT
| ....................TTTTTTTTTTT
| ....................TTTTTTTT#TT
| ....................TTTTTTTTTTT
| ....................TTTTTTTTTTT
| ....................TTTTTTTTTTT

In this diagram, S is the probe's initial position, 0,0. The x coordinate
increases to the right, and the y coordinate increases upward. In the bottom
right, positions that are within the target area are shown as T. After each
step (until the target area is reached), the position of the probe is marked
with #. (The bottom-right # is both a position the probe reaches and a position
in the target area.)

Another initial velocity that causes the probe to be within the target area
after any step is 6,3:

| ...............#..#............
| ...........#........#..........
| ...............................
| ......#..............#.........
| ...............................
| ...............................
| S....................#.........
| ...............................
| ...............................
| ...............................
| .....................#.........
| ....................TTTTTTTTTTT
| ....................TTTTTTTTTTT
| ....................TTTTTTTTTTT
| ....................TTTTTTTTTTT
| ....................T#TTTTTTTTT
| ....................TTTTTTTTTTT

Another one is 9,0:

| S........#.....................
| .................#.............
| ...............................
| ........................#......
| ...............................
| ....................TTTTTTTTTTT
| ....................TTTTTTTTTT#
| ....................TTTTTTTTTTT
| ....................TTTTTTTTTTT
| ....................TTTTTTTTTTT
| ....................TTTTTTTTTTT

One initial velocity that doesn't cause the probe to be within the target area
after any step is 17,-4:

| S..............................................................
| ...............................................................
| ...............................................................
| ...............................................................
| .................#.............................................
| ....................TTTTTTTTTTT................................
| ....................TTTTTTTTTTT................................
| ....................TTTTTTTTTTT................................
| ....................TTTTTTTTTTT................................
| ....................TTTTTTTTTTT..#.............................
| ....................TTTTTTTTTTT................................
| ...............................................................
| ...............................................................
| ...............................................................
| ...............................................................
| ................................................#..............
| ...............................................................
| ...............................................................
| ...............................................................
| ...............................................................
| ...............................................................
| ...............................................................
| ..............................................................#

The probe appears to pass through the target area, but is never within it after
any step. Instead, it continues down and to the right - only the first few
steps are shown.

If you're going to fire a highly scientific probe out of a super cool probe
launcher, you might as well do it with style. How high can you make the probe
go while still reaching the target area?

In the above example, using an initial velocity of 6,9 is the best you can do,
causing the probe to reach a maximum y position of 45. (Any higher initial y
velocity causes the probe to overshoot the target area entirely.)

Find the initial velocity that causes the probe to reach the highest y position
and still eventually be within the target area after any step. What is the
highest y position it reaches on this trajectory?
-}

-- The highest y position is vy+(vy-1)+..1 = vy*(vy+1)/2
-- When returning to y = 0, the y velocity is -vy, so the next y is -vy-1.
-- If lowest part of the the target area is less than zero, then
-- that gives the maximum vy to hit the bottom of the target area vy=-ymin-1.
-- 
-- If the highest part of the target area is more than zero, then that
-- gives the maximum vy to hit to top of the target area vy=ymax

parse :: String -> ((Int,Int),(Int,Int))
parse str = ((read x1,read x2),(read y1,read y2))
  where
    (x1,str1) = span (/= '.') $ drop 1 $ dropWhile (/= '=') str
    (x2,str2) = span (/= ',') $ drop 2 str1
    (y1,str3) = span (/= '.') $ drop 1 $ dropWhile (/= '=') str2
    y2 = drop 2 str3

maxy :: ((Int,Int),(Int,Int)) -> Int
maxy (_,(y1,y2)) = vy*(vy+1) `div` 2
  where vy = max (max y1 y2) (-1 - min y1 y2)

testData :: String
testData = "target area: x=20..30, y=-10..-5"

test :: ()
test
  | (maxy . parse) testData /= 45 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (maxy . parse) $ readFile "input/17.txt"

-- I'll assume 0 < x1 < x2, and y1 < y2 < 0.

vyRange :: ((Int,Int),(Int,Int)) -> (Int,Int)
vyRange (_,(y1,y2)) = (y1,-y1-1)

-- min vx is vx+(vx-1)+..+3+2+1 = x1 = vx*(vx+1)/2
-- vx^2+vx-2*x1 = 0
-- vx = -1/2 + sqrt(2*x1+1/4)
vxRange :: ((Int,Int),(Int,Int)) -> (Int,Int)
vxRange ((x1,x2),_) = (ceiling(-1/2+sqrt(fromIntegral(2*x1)+1/4)),x2)

-- first time y <= y2 to last time y >= y1
-- y(t) = vy + (vy-1) + (vy-2) + ... (vy-(t-1)) = t*vy - t*(t-1)/2
-- t = (2*vy+1)/2 + sqrt((2*vy+1)^2/4 - 2*y(t))
tRangeForVy :: ((Int,Int),(Int,Int)) -> Int -> (Int,Int)
tRangeForVy input@(_,(y1,y2)) vy
  | vy > 0 = let (t1,t2) = tRangeForVy input (-vy-1) in (2*vy+1+t1,2*vy+1+t2)
  | otherwise = (
      ceiling((2*fromIntegral vy+1)/2+sqrt((2*fromIntegral vy+1)^2/4-2*fromIntegral y2)),
      floor((2*fromIntegral vy+1)/2+sqrt((2*fromIntegral vy+1)^2/4-2*fromIntegral y1)))

-- first time x >= x1 to last time x <= x2 or maxT
-- t = (2*vx+1)/2 - sqrt((2*vx+1)^2/4 - 2*x(t))
tRangeForVx :: ((Int,Int),(Int,Int)) -> Int -> Int -> (Int,Int)
tRangeForVx ((x1,x2),_) maxT vx
  | maxX < x1 = (maxT+1,maxT)
  | maxX <= x2 = (t1,maxT)
  | otherwise = (t1,t2)
  where
    maxX = vx*(vx+1) `div` 2
    t1 = ceiling((2*fromIntegral vx+1)/2-sqrt((2*fromIntegral vx+1)^2/4-2*fromIntegral x1))
    t2 = floor((2*fromIntegral vx+1)/2-sqrt((2*fromIntegral vx+1)^2/4-2*fromIntegral x2))

onTarget :: ((Int,Int),(Int,Int)) -> Int -> Int -> Bool
onTarget input vx vy =
    (ty2 >= ty1) && (tx2 >= tx2) && (tx1 <= ty2) && (ty1 <= tx2)
  where
    (ty1,ty2) = tRangeForVy input vy
    (tx1,tx2) = tRangeForVx input ty2 vx

run2 :: String -> [(Int,Int)]
run2 str = [(vx,vy) | vx <- [vxMin..vxMax], vy <- [vyMin..vyMax], onTarget input vx vy]
  where
    input = parse str
    (vyMin,vyMax) = vyRange input
    (vxMin,vxMax) = vxRange input

test2 :: ()
test2
  | (length . run2) testData /= 112 = error "a"
  | otherwise = ()

--part2 :: IO Int
--part2 = fmap (length . run2) $ readFile "input/17.txt"

-- I'm not seeing the error!

-- Brute force to see if I can find a test case.  There are 20 cases
-- for my input data.
targetTimesSlow :: ((Int,Int),(Int,Int)) -> Int -> Int -> [Int]
targetTimesSlow ((x1,x2),(y1,y2)) vx vy = map getT targetSteps
  where
    step (t,x,y,vx,vy) = (t+1,x+vx,y+vy,if vx > 0 then vx-1 else 0,vy-1)
    notPastRange (t,x,y,vx,vy) = x <= x2 && y >= y1
    steps = takeWhile notPastRange $ iterate step (0,0,0,vx,vy)
    inTarget (t,x,y,vx,vy) = x >= x1 && x <= x2 && y >= y1 && y <= y2
    targetSteps = filter inTarget steps
    getT (t,x,y,vx,vy) = t

-- Brute force is fast enough.  I made this more difficult than I had to.

run3 :: String -> [(Int,Int)]
run3 str = [(vx,vy) | vx <- [vxMin..vxMax], vy <- [vyMin..vyMax], not $ null $ targetTimesSlow input vx vy]
  where
    input = parse str
    (vyMin,vyMax) = vyRange input
    (vxMin,vxMax) = vxRange input

test3 :: ()
test3
  | (length . run3) testData /= 112 = error "a"
  | otherwise = ()


part2 :: IO Int
part2 = fmap (length . run3) $ readFile "input/17.txt"
