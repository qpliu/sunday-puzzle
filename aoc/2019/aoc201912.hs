{-
--- Day 12: The N-Body Problem ---

The space near Jupiter is not a very safe place; you need to be careful of a
big distracting red spot, extreme radiation, and a whole lot of moons swirling
around. You decide to start by tracking the four largest moons: Io, Europa,
Ganymede, and Callisto.

After a brief scan, you calculate the position of each moon (your puzzle
input). You just need to simulate their motion so you can avoid them.

Each moon has a 3-dimensional position (x, y, and z) and a 3-dimensional
velocity. The position of each moon is given in your scan; the x, y, and z
velocity of each moon starts at 0.

Simulate the motion of the moons in time steps. Within each time step, first
update the velocity of every moon by applying gravity. Then, once all moons'
velocities have been updated, update the position of every moon by applying
velocity. Time progresses by one step once all of the positions are updated.

To apply gravity, consider every pair of moons. On each axis (x, y, and z), the
velocity of each moon changes by exactly +1 or -1 to pull the moons together.
For example, if Ganymede has an x position of 3, and Callisto has a x position
of 5, then Ganymede's x velocity changes by +1 (because 5 > 3) and Callisto's x
velocity changes by -1 (because 3 < 5). However, if the positions on a given
axis are the same, the velocity on that axis does not change for that pair of
moons.

Once all gravity has been applied, apply velocity: simply add the velocity of
each moon to its own position. For example, if Europa has a position of x=1,
y=2, z=3 and a velocity of x=-2, y=0,z=3, then its new position would be x=-1,
y=2, z=6. This process does not modify the velocity of any moon.

For example, suppose your scan reveals the following positions:

| <x=-1, y=0, z=2>
| <x=2, y=-10, z=-7>
| <x=4, y=-8, z=8>
| <x=3, y=5, z=-1>

Simulating the motion of these moons would produce the following:

| After 0 steps:
| pos=<x=-1, y=  0, z= 2>, vel=<x= 0, y= 0, z= 0>
| pos=<x= 2, y=-10, z=-7>, vel=<x= 0, y= 0, z= 0>
| pos=<x= 4, y= -8, z= 8>, vel=<x= 0, y= 0, z= 0>
| pos=<x= 3, y=  5, z=-1>, vel=<x= 0, y= 0, z= 0>
| 
| After 1 step:
| pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>
| pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>
| pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>
| pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>
| 
| After 2 steps:
| pos=<x= 5, y=-3, z=-1>, vel=<x= 3, y=-2, z=-2>
| pos=<x= 1, y=-2, z= 2>, vel=<x=-2, y= 5, z= 6>
| pos=<x= 1, y=-4, z=-1>, vel=<x= 0, y= 3, z=-6>
| pos=<x= 1, y=-4, z= 2>, vel=<x=-1, y=-6, z= 2>
| 
| After 3 steps:
| pos=<x= 5, y=-6, z=-1>, vel=<x= 0, y=-3, z= 0>
| pos=<x= 0, y= 0, z= 6>, vel=<x=-1, y= 2, z= 4>
| pos=<x= 2, y= 1, z=-5>, vel=<x= 1, y= 5, z=-4>
| pos=<x= 1, y=-8, z= 2>, vel=<x= 0, y=-4, z= 0>
| 
| After 4 steps:
| pos=<x= 2, y=-8, z= 0>, vel=<x=-3, y=-2, z= 1>
| pos=<x= 2, y= 1, z= 7>, vel=<x= 2, y= 1, z= 1>
| pos=<x= 2, y= 3, z=-6>, vel=<x= 0, y= 2, z=-1>
| pos=<x= 2, y=-9, z= 1>, vel=<x= 1, y=-1, z=-1>
| 
| After 5 steps:
| pos=<x=-1, y=-9, z= 2>, vel=<x=-3, y=-1, z= 2>
| pos=<x= 4, y= 1, z= 5>, vel=<x= 2, y= 0, z=-2>
| pos=<x= 2, y= 2, z=-4>, vel=<x= 0, y=-1, z= 2>
| pos=<x= 3, y=-7, z=-1>, vel=<x= 1, y= 2, z=-2>
| 
| After 6 steps:
| pos=<x=-1, y=-7, z= 3>, vel=<x= 0, y= 2, z= 1>
| pos=<x= 3, y= 0, z= 0>, vel=<x=-1, y=-1, z=-5>
| pos=<x= 3, y=-2, z= 1>, vel=<x= 1, y=-4, z= 5>
| pos=<x= 3, y=-4, z=-2>, vel=<x= 0, y= 3, z=-1>
| 
| After 7 steps:
| pos=<x= 2, y=-2, z= 1>, vel=<x= 3, y= 5, z=-2>
| pos=<x= 1, y=-4, z=-4>, vel=<x=-2, y=-4, z=-4>
| pos=<x= 3, y=-7, z= 5>, vel=<x= 0, y=-5, z= 4>
| pos=<x= 2, y= 0, z= 0>, vel=<x=-1, y= 4, z= 2>
| 
| After 8 steps:
| pos=<x= 5, y= 2, z=-2>, vel=<x= 3, y= 4, z=-3>
| pos=<x= 2, y=-7, z=-5>, vel=<x= 1, y=-3, z=-1>
| pos=<x= 0, y=-9, z= 6>, vel=<x=-3, y=-2, z= 1>
| pos=<x= 1, y= 1, z= 3>, vel=<x=-1, y= 1, z= 3>
| 
| After 9 steps:
| pos=<x= 5, y= 3, z=-4>, vel=<x= 0, y= 1, z=-2>
| pos=<x= 2, y=-9, z=-3>, vel=<x= 0, y=-2, z= 2>
| pos=<x= 0, y=-8, z= 4>, vel=<x= 0, y= 1, z=-2>
| pos=<x= 1, y= 1, z= 5>, vel=<x= 0, y= 0, z= 2>
| 
| After 10 steps:
| pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>
| pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>
| pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>
| pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>

Then, it might help to calculate the total energy in the system. The total
energy for a single moon is its potential energy multiplied by its kinetic
energy. A moon's potential energy is the sum of the absolute values of its x,
y, and z position coordinates. A moon's kinetic energy is the sum of the
absolute values of its velocity coordinates. Below, each line shows the
calculations for a moon's potential energy (pot), kinetic energy (kin), and
total energy:

| Energy after 10 steps:
| pot: 2 + 1 + 3 =  6;   kin: 3 + 2 + 1 = 6;   total:  6 * 6 = 36
| pot: 1 + 8 + 0 =  9;   kin: 1 + 1 + 3 = 5;   total:  9 * 5 = 45
| pot: 3 + 6 + 1 = 10;   kin: 3 + 2 + 3 = 8;   total: 10 * 8 = 80
| pot: 2 + 0 + 4 =  6;   kin: 1 + 1 + 1 = 3;   total:  6 * 3 = 18
| Sum of total energy: 36 + 45 + 80 + 18 = 179

In the above example, adding together the total energy for all moons after 10
steps produces the total energy in the system, 179.

Here's a second example:

| <x=-8, y=-10, z=0>
| <x=5, y=5, z=10>
| <x=2, y=-7, z=3>
| <x=9, y=-8, z=-3>

Every ten steps of simulation for 100 steps produces:

| After 0 steps:
| pos=<x= -8, y=-10, z=  0>, vel=<x=  0, y=  0, z=  0>
| pos=<x=  5, y=  5, z= 10>, vel=<x=  0, y=  0, z=  0>
| pos=<x=  2, y= -7, z=  3>, vel=<x=  0, y=  0, z=  0>
| pos=<x=  9, y= -8, z= -3>, vel=<x=  0, y=  0, z=  0>
| 
| After 10 steps:
| pos=<x= -9, y=-10, z=  1>, vel=<x= -2, y= -2, z= -1>
| pos=<x=  4, y= 10, z=  9>, vel=<x= -3, y=  7, z= -2>
| pos=<x=  8, y=-10, z= -3>, vel=<x=  5, y= -1, z= -2>
| pos=<x=  5, y=-10, z=  3>, vel=<x=  0, y= -4, z=  5>
| 
| After 20 steps:
| pos=<x=-10, y=  3, z= -4>, vel=<x= -5, y=  2, z=  0>
| pos=<x=  5, y=-25, z=  6>, vel=<x=  1, y=  1, z= -4>
| pos=<x= 13, y=  1, z=  1>, vel=<x=  5, y= -2, z=  2>
| pos=<x=  0, y=  1, z=  7>, vel=<x= -1, y= -1, z=  2>
| 
| After 30 steps:
| pos=<x= 15, y= -6, z= -9>, vel=<x= -5, y=  4, z=  0>
| pos=<x= -4, y=-11, z=  3>, vel=<x= -3, y=-10, z=  0>
| pos=<x=  0, y= -1, z= 11>, vel=<x=  7, y=  4, z=  3>
| pos=<x= -3, y= -2, z=  5>, vel=<x=  1, y=  2, z= -3>
| 
| After 40 steps:
| pos=<x= 14, y=-12, z= -4>, vel=<x= 11, y=  3, z=  0>
| pos=<x= -1, y= 18, z=  8>, vel=<x= -5, y=  2, z=  3>
| pos=<x= -5, y=-14, z=  8>, vel=<x=  1, y= -2, z=  0>
| pos=<x=  0, y=-12, z= -2>, vel=<x= -7, y= -3, z= -3>
| 
| After 50 steps:
| pos=<x=-23, y=  4, z=  1>, vel=<x= -7, y= -1, z=  2>
| pos=<x= 20, y=-31, z= 13>, vel=<x=  5, y=  3, z=  4>
| pos=<x= -4, y=  6, z=  1>, vel=<x= -1, y=  1, z= -3>
| pos=<x= 15, y=  1, z= -5>, vel=<x=  3, y= -3, z= -3>
| 
| After 60 steps:
| pos=<x= 36, y=-10, z=  6>, vel=<x=  5, y=  0, z=  3>
| pos=<x=-18, y= 10, z=  9>, vel=<x= -3, y= -7, z=  5>
| pos=<x=  8, y=-12, z= -3>, vel=<x= -2, y=  1, z= -7>
| pos=<x=-18, y= -8, z= -2>, vel=<x=  0, y=  6, z= -1>
| 
| After 70 steps:
| pos=<x=-33, y= -6, z=  5>, vel=<x= -5, y= -4, z=  7>
| pos=<x= 13, y= -9, z=  2>, vel=<x= -2, y= 11, z=  3>
| pos=<x= 11, y= -8, z=  2>, vel=<x=  8, y= -6, z= -7>
| pos=<x= 17, y=  3, z=  1>, vel=<x= -1, y= -1, z= -3>
| 
| After 80 steps:
| pos=<x= 30, y= -8, z=  3>, vel=<x=  3, y=  3, z=  0>
| pos=<x= -2, y= -4, z=  0>, vel=<x=  4, y=-13, z=  2>
| pos=<x=-18, y= -7, z= 15>, vel=<x= -8, y=  2, z= -2>
| pos=<x= -2, y= -1, z= -8>, vel=<x=  1, y=  8, z=  0>
| 
| After 90 steps:
| pos=<x=-25, y= -1, z=  4>, vel=<x=  1, y= -3, z=  4>
| pos=<x=  2, y= -9, z=  0>, vel=<x= -3, y= 13, z= -1>
| pos=<x= 32, y= -8, z= 14>, vel=<x=  5, y= -4, z=  6>
| pos=<x= -1, y= -2, z= -8>, vel=<x= -3, y= -6, z= -9>
| 
| After 100 steps:
| pos=<x=  8, y=-12, z= -9>, vel=<x= -7, y=  3, z=  0>
| pos=<x= 13, y= 16, z= -3>, vel=<x=  3, y=-11, z= -5>
| pos=<x=-29, y=-11, z= -1>, vel=<x= -3, y=  7, z=  4>
| pos=<x= 16, y=-13, z= 23>, vel=<x=  7, y=  1, z=  1>
| 
| Energy after 100 steps:
| pot:  8 + 12 +  9 = 29;   kin: 7 +  3 + 0 = 10;   total: 29 * 10 = 290
| pot: 13 + 16 +  3 = 32;   kin: 3 + 11 + 5 = 19;   total: 32 * 19 = 608
| pot: 29 + 11 +  1 = 41;   kin: 3 +  7 + 4 = 14;   total: 41 * 14 = 574
| pot: 16 + 13 + 23 = 52;   kin: 7 +  1 + 1 =  9;   total: 52 *  9 = 468
| Sum of total energy: 290 + 608 + 574 + 468 = 1940

What is the total energy in the system after simulating the moons given in your
scan for 1000 steps?

-- Part Two ---

All this drifting around in space makes you wonder about the nature of the
universe. Does history really repeat itself? You're curious whether the moons
will ever return to a previous state.

Determine the number of steps that must occur before all of the moons'
positions and velocities exactly match a previous point in time.

For example, the first example above takes 2772 steps before they exactly match
a previous point in time; it eventually returns to the initial state:

| After 0 steps:
| pos=<x= -1, y=  0, z=  2>, vel=<x=  0, y=  0, z=  0>
| pos=<x=  2, y=-10, z= -7>, vel=<x=  0, y=  0, z=  0>
| pos=<x=  4, y= -8, z=  8>, vel=<x=  0, y=  0, z=  0>
| pos=<x=  3, y=  5, z= -1>, vel=<x=  0, y=  0, z=  0>
| 
| After 2770 steps:
| pos=<x=  2, y= -1, z=  1>, vel=<x= -3, y=  2, z=  2>
| pos=<x=  3, y= -7, z= -4>, vel=<x=  2, y= -5, z= -6>
| pos=<x=  1, y= -7, z=  5>, vel=<x=  0, y= -3, z=  6>
| pos=<x=  2, y=  2, z=  0>, vel=<x=  1, y=  6, z= -2>
| 
| After 2771 steps:
| pos=<x= -1, y=  0, z=  2>, vel=<x= -3, y=  1, z=  1>
| pos=<x=  2, y=-10, z= -7>, vel=<x= -1, y= -3, z= -3>
| pos=<x=  4, y= -8, z=  8>, vel=<x=  3, y= -1, z=  3>
| pos=<x=  3, y=  5, z= -1>, vel=<x=  1, y=  3, z= -1>
| 
| After 2772 steps:
| pos=<x= -1, y=  0, z=  2>, vel=<x=  0, y=  0, z=  0>
| pos=<x=  2, y=-10, z= -7>, vel=<x=  0, y=  0, z=  0>
| pos=<x=  4, y= -8, z=  8>, vel=<x=  0, y=  0, z=  0>
| pos=<x=  3, y=  5, z= -1>, vel=<x=  0, y=  0, z=  0>

Of course, the universe might last for a very long time before repeating.
Here's a copy of the second example from above:

| <x=-8, y=-10, z=0>
| <x=5, y=5, z=10>
| <x=2, y=-7, z=3>
| <x=9, y=-8, z=-3>

This set of initial positions takes 4686774924 steps before it repeats a
previous state! Clearly, you might need to find a more efficient way to
simulate the universe.

How many steps does it take to reach the first state that exactly matches a
previous state?
-}

import Data.Char(isDigit)
import Data.List(partition)

type Moon = ((Int,Int,Int),(Int,Int,Int))

parse :: String -> [Moon]
parse = p . map (read . filter isNum) . words
  where
    isNum c = c == '-' || isDigit c
    p (px:py:pz:rest) = ((px,py,pz),(0,0,0)) : p rest
    p _ = []

energy :: Moon -> (Int,(Int,Int))
energy ((px,py,pz),(vx,vy,vz)) = (pot*kin,(pot,kin))
  where
    pot = abs px+abs py+abs pz
    kin = abs vx+abs vy+abs vz

gravity :: [Moon] -> [Moon]
gravity [(p0@(px0,py0,pz0),(vx0,vy0,vz0)),
         (p1@(px1,py1,pz1),(vx1,vy1,vz1)),
         (p2@(px2,py2,pz2),(vx2,vy2,vz2)),
         (p3@(px3,py3,pz3),(vx3,vy3,vz3))] =
    [(p0,(vx0+delta px0 [px1,px2,px3],
          vy0+delta py0 [py1,py2,py3],
          vz0+delta pz0 [pz1,pz2,pz3])),
     (p1,(vx1+delta px1 [px0,px2,px3],
          vy1+delta py1 [py0,py2,py3],
          vz1+delta pz1 [pz0,pz2,pz3])),
     (p2,(vx2+delta px2 [px0,px1,px3],
          vy2+delta py2 [py0,py1,py3],
          vz2+delta pz2 [pz0,pz1,pz3])),
     (p3,(vx3+delta px3 [px0,px1,px2],
          vy3+delta py3 [py0,py1,py2],
          vz3+delta pz3 [pz0,pz1,pz2]))]
  where
    delta p ps = length plus - length (filter (/= p) notPlus)
      where (plus,notPlus) = partition (> p) ps

velocity :: Moon -> Moon
velocity ((px,py,pz),v@(vx,vy,vz)) = ((px+vx,py+vy,pz+vz),v)

testData :: [((Int,Int),String)]
testData = [
    ((10,179),"<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>"),
    ((100,1940),"<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>")
    ]

test :: ()
test
  | not (all testItem testData) = error "a"
  | otherwise = ()
  where
    testItem ((nsteps,expectedEnergy),input) =
        expectedEnergy ==  sum (map (fst . energy) $ head $ drop nsteps $ iterate (map velocity . gravity) $ parse input)

part1 :: IO Int
part1 = fmap (sum . map (fst . energy) . head . drop 1000 . iterate (map velocity . gravity) . parse) $ readFile "input/12.txt"

-- Part 2: Since x, y, and z are independent, find the period for each
-- one separately.

step2 :: (Int,Int,Int,Int,Int,Int,Int,Int) -> (Int,Int,Int,Int,Int,Int,Int,Int)
step2 (x0,v0,x1,v1,x2,v2,x3,v3) = (x0+v0+dv0,v0+dv0,x1+v1+dv1,v1+dv1,x2+v2+dv2,v2+dv2,x3+v3+dv3,v3+dv3)
  where
    dv0 = delta x0 [x1,x2,x3]
    dv1 = delta x1 [x0,x2,x3]
    dv2 = delta x2 [x0,x1,x3]
    dv3 = delta x3 [x0,x1,x2]
    delta x xs = length plus - length (filter (/= x) notPlus)
      where (plus,notPlus) = partition (> x) xs

cycleLength :: (Int,Int,Int,Int) -> Int
cycleLength (x0,x1,x2,x3) = count 1 (step2 p0)
  where
    p0 = (x0,0,x1,0,x2,0,x3,0)
    count n p
      | p == p0 = n
      | otherwise = count (n+1) (step2 p)

period :: [Moon] -> Int
period [((x0,y0,z0),_),((x1,y1,z1),_),((x2,y2,z2),_),((x3,y3,z3),_)] = cxyz
  where
    cx = cycleLength (x0,x1,x2,x3)
    cy = cycleLength (y0,y1,y2,y3)
    cz = cycleLength (z0,z1,z2,z3)
    cxy = cx*cy `div` gcd cx cy
    cxyz = cxy*cz `div` gcd cxy cz

test2 :: ()
test2
  | (period $ parse $ snd $ testData!!0) /= 2772 = error "a"
  | (period $ parse $ snd $ testData!!1) /= 4686774924 = error "b"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (period . parse) $ readFile "input/12.txt"
