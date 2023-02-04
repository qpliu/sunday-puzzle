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
