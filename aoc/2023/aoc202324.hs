type Hailstone = ((Rational,Rational,Rational),(Rational,Rational,Rational))

parse :: String -> [Hailstone]
parse = map (parseLine . map (fromIntegral . read) . words) . lines . filter (not . (`elem` ",@"))
  where parseLine [x,y,z,vx,vy,vz] = ((x,y,z),(vx,vy,vz))
-- x1i + vx1*t1 = x2i + vx2*t2
-- y1i + vy1*t1 = y2i + vy2*t2
-- if vy1 = 0, swap 1 and 2
-- y1i*vx1/vy1 + vx1*t1 = y2i*vx1/vy1 + vy2*vx1/vy1*t2
-- x1i - y1i*vx1/vy1 - x2i + y2i*vx1/vy1 = (vx2 - vy2*vx1/vy1)*t2
-- if (vx2 - vy2*vx1/vy1) = 0, parallel
-- t2 = (x1i - y1i*vx1/vy1 - x2i + y2i*vx1/vy1)/(vx2 - vy2*vx1/vy1)
-- t1 = (y2i + vy2*t2 - y1i)/vy1
-- if t1 < 0 or t2 < 0, in the past
-- x = x1i + vx1*t1
-- y = y1i + vy1*t1

futurePathsCross :: (Rational,Rational) -> Hailstone -> Hailstone -> Bool
futurePathsCross (minXY,maxXY) h1@((x1i,y1i,_),(vx1,vy1,_)) h2@((x2i,y2i,_),(vx2,vy2,_))
  | vy1 == 0 && vy2 == 0 = False -- parallel
  | vy1 == 0 = futurePathsCross (minXY,maxXY) h2 h1
  | (vx2 - vy2*vx1/vy1) == 0 = False -- parallel
  | t1 < 0 || t2 < 0 = False -- past
  | otherwise = minXY <= x && x <= maxXY && minXY <= y && y <= maxXY
  where
    t2 = (x1i - y1i*vx1/vy1 - x2i + y2i*vx1/vy1)/(vx2 - vy2*vx1/vy1)
    t1 = (y2i + vy2*t2 - y1i)/vy1
    x = x1i + vx1*t1
    y = y1i + vy1*t1

result :: (Rational,Rational) -> String -> Int
result testArea input = length [(h1,h2) | h1 <- hailstones, h2 <- hailstones, h1 > h2, futurePathsCross testArea h1 h2]
  where
    hailstones = parse input

testData :: String
testData = unlines [
    "19, 13, 30 @ -2,  1, -2",
    "18, 19, 22 @ -1, -1, -2",
    "20, 25, 34 @ -2, -2, -4",
    "12, 31, 28 @ -1, -2, -1",
    "20, 19, 15 @  1, -5, -3"
    ]

test :: ()
test
  | result (7,27) testData /= 2 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (result (200000000000000,400000000000000)) $ readFile "input/24.txt"

{-
If there is a solution, any 3 hailstones should determine the rock
 x + u*t1 = x1 + u1*t1
 y + v*t1 = y1 + v1*t1
 z + w*t1 = z1 + w1*t1
 x + u*t2 = x2 + u2*t2
 y + v*t2 = y2 + v2*t2
 z + w*t2 = z2 + w2*t2
 x + u*t3 = x3 + u3*t3
 y + v*t3 = y3 + v3*t3
 z + w*t3 = z3 + w3*t3
9 nonlinear equations with 9 unknowns.

Probably easier to just do 2 of the coordinates at a time -- need 4 hailstones
 x + u*t1 = x1 + u1*t1
 y + v*t1 = y1 + v1*t1
 x + u*t2 = x2 + u2*t2
 y + v*t2 = y2 + v2*t2
 x + u*t3 = x3 + u3*t3
 y + v*t3 = y3 + v3*t3
 x + u*t4 = x4 + u4*t4
 y + v*t4 = y4 + v4*t4
8 nonlinear equations for 8 unknowns: x y u v t1 t2 t3 t4

substitute for t1 t2 t3 t4 gives 4 equations for 4 unknowns: x y u v

  t1 = (x1-x)/(u-u1)

  (y-y1)*(u-u1) = (v-v1)*(x-x1)
  (y-y2)*(u-u2) = (v-v2)*(x-x2)
  (y-y3)*(u-u3) = (v-v3)*(x-x3)
  (y-y4)*(u-u4) = (v-v4)*(x-x4)

eliminate the nonlinear terms by subtracting pairs of equations:

  y*(-u1+u2)+u*(-y1+y2)+y1*u1-y2*u2 = x*(-v1+v2)+v*(-x1+x2)+v1*x1-v2*x2

  (v1-v2)*x-(y1-y2)*u-(u1-u2)*y+(x1-x2)*v = v1*x1-v2*x2-u1*y1+u2*y2
  (v2-v3)*x-(y2-y3)*u-(u2-u3)*y+(x2-x3)*v = v2*x2-v3*x3-u2*y2+u3*y3
  (v3-v4)*x-(y3-y4)*u-(u3-u4)*y+(x3-x4)*v = v3*x3-v4*x4-u3*y3+u4*y4
  (v4-v1)*x-(y4-y1)*u-(u4-u1)*y+(x4-x1)*v = v4*x4-v1*x1-u4*y4+u1*y1

solve for x, u, y, and v

then, plug the solutions for x and u into:

  (w1-w2)*x-(z1-z2)*u-(u1-u2)*z+(x1-x2)*w = w1*x1-w2*x2-u1*z1+u2*z2
  (w2-w3)*x-(z2-z3)*u-(u2-u3)*z+(x2-x3)*w = w2*x2-w3*x3-u2*z2+u3*z3

solve for z and w
-}

solve :: [Hailstone] -> ([Rational],Hailstone)
solve (((x1,y1,z1),(u1,v1,w1)):((x2,y2,z2),(u2,v2,w2)):((x3,y3,z3),(u3,v3,w3)):((x4,y4,z4),(u4,v4,w4)):((x5,y5,z5),(u5,v5,w5)):_) = ([x,y,z],((x,y,z),(u,v,w)))
  where
    [x,u,y,v] = reverse $ backsubst $ reverse $ map reverse $ upperdiag [
        [v1-v2,y2-y1,u2-u1,x1-x2,v1*x1-v2*x2-u1*y1+u2*y2],
        [v2-v3,y3-y2,u3-u2,x2-x3,v2*x2-v3*x3-u2*y2+u3*y3],
        [v3-v4,y4-y3,u4-u3,x3-x4,v3*x3-v4*x4-u3*y3+u4*y4],
        [v4-v5,y5-y4,u5-u4,x4-x5,v4*x4-v5*x5-u4*y4+u5*y5]
        ]
    [z,w] = reverse $ backsubst $ reverse $ map reverse $ upperdiag [
        [u2-u1,x1-x2,w1*x1-w2*x2-u1*z1+u2*z2+(w2-w1)*x+(z1-z2)*u],
        [u3-u2,x2-x3,w2*x2-w3*x3-u2*z2+u3*z3+(w3-w2)*x+(z2-z3)*u]
        ]
    upperdiag :: [[Rational]] -> [[Rational]]
    upperdiag [row] = [row]
    upperdiag (row@(a:as):rows)
      | a == 0 = upperdiag (rows ++ [row])
      | otherwise = row:upperdiag (map f rows)
      where f (b:bs) = zipWith (-) bs (map (* (b/a)) as)
    backsubst :: [[Rational]] -> [Rational]
    backsubst [] = []
    backsubst (([a1,a2]):rows) = (a1/a2) : backsubst (map subst rows)
      where subst (b1:b2:bs) = (b1-b2*a1/a2):bs

result2 :: String -> Rational
result2 = sum . fst . solve . parse

test2 :: ()
test2
  | result2 testData /= 47 = error "a"
  | otherwise = ()

part2 :: IO Rational
part2 = fmap result2 $ readFile "input/24.txt"
