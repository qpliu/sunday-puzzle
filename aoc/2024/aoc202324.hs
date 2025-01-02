module AOC202324 where

import Debug.Trace(traceShow)

import AOC

aoc = AOC {
    day="../../2023/input/24",
    aocTests=[
        AOCTest {
            testData=unlines [
                "19, 13, 30 @ -2,  1, -2",
                "18, 19, 22 @ -1, -1, -2",
                "20, 25, 34 @ -2, -2, -4",
                "12, 31, 28 @ -1, -2, -1",
                "20, 19, 15 @  1, -5, -3"
                ],
            testResult=Just "2",
            testResult2=Just "47"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result (7,27),
        pcodeTest2=const result2,
        pcodeResult=result (200000000000000,400000000000000),
        pcodeResult2=const result2
        }
    }

parse :: String -> [[Rational]]
parse = map (map fromIntegral . parseInts) . lines

collides :: (Rational,Rational) -> ([Rational],[Rational]) -> Int
collides (testMin,testMax) ([x1,y1,z1,vx1,vy1,vz1],[x2,y2,z2,vx2,vy2,vz2])
  | vx2*vy1 - vx1*vy2 == 0 = 0 -- parallel
  | x < testMin || x > testMax || y < testMin || y > testMax = 0
  | t1 < 0 || t2 < 0 = 0
  | otherwise = 1
  where
    x = (vx2*vy1*x1 - vx1*vx2*y1 - (vy2*x2 - vx2*y2)*vx1) / (vx2*vy1 - vx1*vy2)
    y = (vy2*vx1*y1 - vy1*vy2*x1 - (vx2*y2 - vy2*x2)*vy1) / (vy2*vx1 - vy1*vx2)
    t1 | vx1 /= 0 = (x - x1) / vx1
       | otherwise = (y - y1) / vy1
    t2 | vx2 /= 0 = (x - x2) / vx2
       | otherwise = (y - y2) / vy2

result testArea ncpu stones =
    parallelMapReduce ncpu (collides testArea) sum
                      [(a,b) | a <- stones, b <- stones, a > b]

-- Part 2 solution using Sage (sagemath.org):
{-
(xc,yc,zc,uc,vc,wc) = var('x,y,z,u,v,w')

x = [var(f"x{i+1}") for i in [0..4]]
y = [var(f"y{i+1}") for i in [0..4]]
z = [var(f"z{i+1}") for i in [0..4]]
u = [var(f"u{i+1}") for i in [0..4]]
v = [var(f"v{i+1}") for i in [0..4]]
w = [var(f"w{i+1}") for i in [0..4]]
t = [var(f"t{i+1}") for i in [0..4]]

ex1 = [xc+uc*t[i] == x[i]+u[i]*t[i] for i in [0..4]]
ey1 = [yc+vc*t[i] == y[i]+v[i]*t[i] for i in [0..4]]
ez1 = [zc+wc*t[i] == z[i]+w[i]*t[i] for i in [0..4]]

exy2 = [ex1[i].substitute(solve(ey1[i],t[i]))*(vc-v[i]) for i in [0..4]]
eyz2 = [ey1[i].substitute(solve(ez1[i],t[i]))*(wc-w[i]) for i in [0..4]]

exy3 = [(exy2[i]-exy2[0]).simplify_full() for i in [1..4]]
eyz3 = [(eyz2[i]-eyz2[0]).simplify_full() for i in [1..4]]

sxy = solve(exy3, [xc,yc,uc,vc])
syz = solve(eyz3, [yc,zc,vc,wc])

sx = sxy[0][0]
sy = sxy[0][1]
sz = syz[0][1]

print(sx)
print(sy)
print(sz)
-}

solve :: [[Rational]] -> [Rational]
solve ([x1,y1,z1,u1,v1,w1]:[x2,y2,z2,u2,v2,w2]:[x3,y3,z3,u3,v3,w3]
       :[x4,y4,z4,u4,v4,w4]:[x5,y5,z5,u5,v5,w5]:_) =
    [x,y,z]
  where
    x = (((v4*x4*y5 + (x5*y4 - x4*y5)*v3 - (v5*x5 + u4*y5 - u5*y5)*y4)*x3 + (u4*x5*y4 - (x5*y4 - x4*y5)*u3 - (v4*x5 - v5*x5 + u5*y5)*x4)*y3)*u1 - ((v4*x4*y5 + (x5*y4 - x4*y5)*v3 - (v5*x5 + u4*y5 - u5*y5)*y4)*x3 + (u4*x5*y4 - (x5*y4 - x4*y5)*u3 - (v4*x5 - v5*x5 + u5*y5)*x4)*y3)*u2 - ((u5*y4 - u4*y5)*v3*x3 - (v3*x3*(y4 - y5) + v4*x4*y5 - (v4*x4 - v5*x5 + u3*(y4 - y5) - u4*y4 + u5*y5)*y3 - (v5*x5 + u4*y5 - u5*y5)*y4)*u2 + (v4*x4*y5 - (v5*x5 + u4*y5 - u5*y5)*y4)*u3 - (((x4 - x5)*y3 - x3*(y4 - y5) + x5*y4 - x4*y5)*u2 - (x5*y4 - x4*y5)*u3 + (u5*y4 - u4*y5)*x3 - (u5*x4 - u4*x5)*y3)*v1 - (u5*v4*x4 - u4*u5*y4 + (u5*y4 - u4*y5)*u3 - (v5*x5 - u5*y5)*u4)*y3)*x1 + ((u5*y4 - u4*y5)*v3*x3 - (v3*x3*(y4 - y5) + v4*x4*y5 - (v4*x4 - v5*x5 + u3*(y4 - y5) - u4*y4 + u5*y5)*y3 - (v5*x5 + u4*y5 - u5*y5)*y4)*u1 + (v4*x4*y5 - (v5*x5 + u4*y5 - u5*y5)*y4)*u3 - (((x4 - x5)*y3 - x3*(y4 - y5) + x5*y4 - x4*y5)*u1 - (x5*y4 - x4*y5)*u3 + (u5*y4 - u4*y5)*x3 - (u5*x4 - u4*x5)*y3)*v2 - (((u4 - u5)*y3 - u3*(y4 - y5) + u5*y4 - u4*y5)*v1 - ((u4 - u5)*y3 - u3*(y4 - y5) + u5*y4 - u4*y5)*v2)*x1 - (u5*v4*x4 - u4*u5*y4 + (u5*y4 - u4*y5)*u3 - (v5*x5 - u5*y5)*u4)*y3)*x2 - ((u5*x4 - u4*x5)*u3*y3 - ((x5*y4 - x4*y5)*u3 - (u5*y4 - u4*y5)*x3 + (u5*x4 - u4*x5)*y3)*u1 - (u3*(x4 - x5)*y3 + u4*x5*y4 - ((x4 - x5)*y3 - x3*(y4 - y5) + x5*y4 - x4*y5)*u1 - (v3*(x4 - x5) - v4*x4 + v5*x5 + u4*y4 - u5*y5)*x3 - (v4*x5 - v5*x5 + u5*y5)*x4)*u2 + (u4*x5*y4 - (v4*x5 - v5*x5 + u5*y5)*x4)*u3 - ((u4 - u5)*v3*x3 + u5*v4*x4 - u3*(u4 - u5)*y3 - u4*u5*y4 + ((u4 - u5)*y3 - u3*(y4 - y5) + u5*y4 - u4*y5)*u1 - (v4*x4 - v5*x5 - u4*y4 + u5*y5)*u3 - (v5*x5 - u5*y5)*u4 - ((u4 - u5)*x3 - u3*(x4 - x5) + u5*x4 - u4*x5)*v2)*x2 + (u5*v4*x4 - u4*u5*y4 - (v5*x5 - u5*y5)*u4 - (u5*x4 - u4*x5)*v3)*x3)*y1 + ((u5*x4 - u4*x5)*u3*y3 - (u3*(x4 - x5)*y3 + u4*x5*y4 - (v3*(x4 - x5) - v4*x4 + v5*x5 + u4*y4 - u5*y5)*x3 - (v4*x5 - v5*x5 + u5*y5)*x4)*u1 + (((x4 - x5)*y3 - x3*(y4 - y5) + x5*y4 - x4*y5)*u1 - (x5*y4 - x4*y5)*u3 + (u5*y4 - u4*y5)*x3 - (u5*x4 - u4*x5)*y3)*u2 + (u4*x5*y4 - (v4*x5 - v5*x5 + u5*y5)*x4)*u3 - ((u4 - u5)*v3*x3 + u5*v4*x4 - u3*(u4 - u5)*y3 - u4*u5*y4 + ((u4 - u5)*y3 - u3*(y4 - y5) + u5*y4 - u4*y5)*u2 - (v4*x4 - v5*x5 - u4*y4 + u5*y5)*u3 - (v5*x5 - u5*y5)*u4 - ((u4 - u5)*x3 - u3*(x4 - x5) + u5*x4 - u4*x5)*v1)*x1 + (u5*v4*x4 - u4*u5*y4 - (v5*x5 - u5*y5)*u4 - (u5*x4 - u4*x5)*v3)*x3 - (((u4 - u5)*x3 - u3*(x4 - x5) + u5*x4 - u4*x5)*u1 - ((u4 - u5)*x3 - u3*(x4 - x5) + u5*x4 - u4*x5)*u2)*y1)*y2)/(((x5*y4 - x4*y5)*v3 - (v5*y4 - v4*y5)*x3 + (v5*x4 - v4*x5)*y3)*u1 - ((x5*y4 - x4*y5)*v3 - (v5*y4 - v4*y5)*x3 + (v5*x4 - v4*x5)*y3)*u2 + (((x4 - x5)*y3 - x3*(y4 - y5) + x5*y4 - x4*y5)*u2 - (x5*y4 - x4*y5)*u3 + (u5*y4 - u4*y5)*x3 - (u5*x4 - u4*x5)*y3)*v1 - (((x4 - x5)*y3 - x3*(y4 - y5) + x5*y4 - x4*y5)*u1 - (x5*y4 - x4*y5)*u3 + (u5*y4 - u4*y5)*x3 - (u5*x4 - u4*x5)*y3)*v2 - (((v4 - v5)*y3 - v3*(y4 - y5) + v5*y4 - v4*y5)*u2 - (v5*y4 - v4*y5)*u3 - ((u4 - u5)*y3 - u3*(y4 - y5) + u5*y4 - u4*y5)*v2 + (u5*y4 - u4*y5)*v3 - (u5*v4 - u4*v5)*y3)*x1 + (((v4 - v5)*y3 - v3*(y4 - y5) + v5*y4 - v4*y5)*u1 - (v5*y4 - v4*y5)*u3 - ((u4 - u5)*y3 - u3*(y4 - y5) + u5*y4 - u4*y5)*v1 + (u5*y4 - u4*y5)*v3 - (u5*v4 - u4*v5)*y3)*x2 + (((v4 - v5)*x3 - v3*(x4 - x5) + v5*x4 - v4*x5)*u2 - (v5*x4 - v4*x5)*u3 - ((u4 - u5)*x3 - u3*(x4 - x5) + u5*x4 - u4*x5)*v2 + (u5*x4 - u4*x5)*v3 + ((u4 - u5)*v3 - u3*(v4 - v5) + u5*v4 - u4*v5)*x2 - (u5*v4 - u4*v5)*x3)*y1 - (((v4 - v5)*x3 - v3*(x4 - x5) + v5*x4 - v4*x5)*u1 - (v5*x4 - v4*x5)*u3 - ((u4 - u5)*x3 - u3*(x4 - x5) + u5*x4 - u4*x5)*v1 + (u5*x4 - u4*x5)*v3 + ((u4 - u5)*v3 - u3*(v4 - v5) + u5*v4 - u4*v5)*x1 - (u5*v4 - u4*v5)*x3)*y2)

    y = (((v4*x4*y5 + (x5*y4 - x4*y5)*v3 - (v5*x5 + u4*y5 - u5*y5)*y4)*x3 + (u4*x5*y4 - (x5*y4 - x4*y5)*u3 - (v4*x5 - v5*x5 + u5*y5)*x4)*y3)*v1 - ((v4*x4*y5 + (x5*y4 - x4*y5)*v3 - (v5*x5 + u4*y5 - u5*y5)*y4)*x3 + (u4*x5*y4 - (x5*y4 - x4*y5)*u3 - (v4*x5 - v5*x5 + u5*y5)*x4)*y3)*v2 - ((v5*y4 - v4*y5)*v3*x3 + ((x5*y4 - x4*y5)*v3 - (v5*y4 - v4*y5)*x3 + (v5*x4 - v4*x5)*y3)*v1 - (v3*x3*(y4 - y5) + v4*x4*y5 + ((x4 - x5)*y3 - x3*(y4 - y5) + x5*y4 - x4*y5)*v1 - (v4*x4 - v5*x5 + u3*(y4 - y5) - u4*y4 + u5*y5)*y3 - (v5*x5 + u4*y5 - u5*y5)*y4)*v2 + (v4*x4*y5 - (v5*x5 + u4*y5 - u5*y5)*y4)*v3 - (v4*v5*x4 - u4*v5*y4 + (v5*y4 - v4*y5)*u3 - (v5*x5 - u5*y5)*v4)*y3)*x1 + ((v5*y4 - v4*y5)*v3*x3 - (v3*x3*(y4 - y5) + v4*x4*y5 - (v4*x4 - v5*x5 + u3*(y4 - y5) - u4*y4 + u5*y5)*y3 - (v5*x5 + u4*y5 - u5*y5)*y4)*v1 - (((x4 - x5)*y3 - x3*(y4 - y5) + x5*y4 - x4*y5)*v1 - (x5*y4 - x4*y5)*v3 + (v5*y4 - v4*y5)*x3 - (v5*x4 - v4*x5)*y3)*v2 + (v4*x4*y5 - (v5*x5 + u4*y5 - u5*y5)*y4)*v3 - (((v4 - v5)*y3 - v3*(y4 - y5) + v5*y4 - v4*y5)*v1 - ((v4 - v5)*y3 - v3*(y4 - y5) + v5*y4 - v4*y5)*v2)*x1 - (v4*v5*x4 - u4*v5*y4 + (v5*y4 - v4*y5)*u3 - (v5*x5 - u5*y5)*v4)*y3)*x2 - ((v5*x4 - v4*x5)*u3*y3 - ((x5*y4 - x4*y5)*v3 - (v5*y4 - v4*y5)*x3 + (v5*x4 - v4*x5)*y3)*u1 - (u3*(x4 - x5)*y3 + u4*x5*y4 - ((x4 - x5)*y3 - x3*(y4 - y5) + x5*y4 - x4*y5)*u1 - (v3*(x4 - x5) - v4*x4 + v5*x5 + u4*y4 - u5*y5)*x3 - (v4*x5 - v5*x5 + u5*y5)*x4)*v2 + (u4*x5*y4 - (v4*x5 - v5*x5 + u5*y5)*x4)*v3 - (v3*(v4 - v5)*x3 + v4*v5*x4 - u3*(v4 - v5)*y3 - u4*v5*y4 + ((v4 - v5)*y3 - v3*(y4 - y5) + v5*y4 - v4*y5)*u1 - ((v4 - v5)*x3 - v3*(x4 - x5) + v5*x4 - v4*x5)*v2 - (v4*x4 - v5*x5 - u4*y4 + u5*y5)*v3 - (v5*x5 - u5*y5)*v4)*x2 + (v4*v5*x4 - u4*v5*y4 - (v5*x4 - v4*x5)*v3 - (v5*x5 - u5*y5)*v4)*x3)*y1 + ((v5*x4 - v4*x5)*u3*y3 - ((x5*y4 - x4*y5)*v3 - (v5*y4 - v4*y5)*x3 + (v5*x4 - v4*x5)*y3)*u2 - (u3*(x4 - x5)*y3 + u4*x5*y4 - ((x4 - x5)*y3 - x3*(y4 - y5) + x5*y4 - x4*y5)*u2 - (v3*(x4 - x5) - v4*x4 + v5*x5 + u4*y4 - u5*y5)*x3 - (v4*x5 - v5*x5 + u5*y5)*x4)*v1 + (u4*x5*y4 - (v4*x5 - v5*x5 + u5*y5)*x4)*v3 - (v3*(v4 - v5)*x3 + v4*v5*x4 - u3*(v4 - v5)*y3 - u4*v5*y4 + ((v4 - v5)*y3 - v3*(y4 - y5) + v5*y4 - v4*y5)*u2 - ((v4 - v5)*x3 - v3*(x4 - x5) + v5*x4 - v4*x5)*v1 - (v4*x4 - v5*x5 - u4*y4 + u5*y5)*v3 - (v5*x5 - u5*y5)*v4)*x1 + (v4*v5*x4 - u4*v5*y4 - (v5*x4 - v4*x5)*v3 - (v5*x5 - u5*y5)*v4)*x3 - (((v4 - v5)*x3 - v3*(x4 - x5) + v5*x4 - v4*x5)*u1 - ((v4 - v5)*x3 - v3*(x4 - x5) + v5*x4 - v4*x5)*u2)*y1)*y2)/(((x5*y4 - x4*y5)*v3 - (v5*y4 - v4*y5)*x3 + (v5*x4 - v4*x5)*y3)*u1 - ((x5*y4 - x4*y5)*v3 - (v5*y4 - v4*y5)*x3 + (v5*x4 - v4*x5)*y3)*u2 + (((x4 - x5)*y3 - x3*(y4 - y5) + x5*y4 - x4*y5)*u2 - (x5*y4 - x4*y5)*u3 + (u5*y4 - u4*y5)*x3 - (u5*x4 - u4*x5)*y3)*v1 - (((x4 - x5)*y3 - x3*(y4 - y5) + x5*y4 - x4*y5)*u1 - (x5*y4 - x4*y5)*u3 + (u5*y4 - u4*y5)*x3 - (u5*x4 - u4*x5)*y3)*v2 - (((v4 - v5)*y3 - v3*(y4 - y5) + v5*y4 - v4*y5)*u2 - (v5*y4 - v4*y5)*u3 - ((u4 - u5)*y3 - u3*(y4 - y5) + u5*y4 - u4*y5)*v2 + (u5*y4 - u4*y5)*v3 - (u5*v4 - u4*v5)*y3)*x1 + (((v4 - v5)*y3 - v3*(y4 - y5) + v5*y4 - v4*y5)*u1 - (v5*y4 - v4*y5)*u3 - ((u4 - u5)*y3 - u3*(y4 - y5) + u5*y4 - u4*y5)*v1 + (u5*y4 - u4*y5)*v3 - (u5*v4 - u4*v5)*y3)*x2 + (((v4 - v5)*x3 - v3*(x4 - x5) + v5*x4 - v4*x5)*u2 - (v5*x4 - v4*x5)*u3 - ((u4 - u5)*x3 - u3*(x4 - x5) + u5*x4 - u4*x5)*v2 + (u5*x4 - u4*x5)*v3 + ((u4 - u5)*v3 - u3*(v4 - v5) + u5*v4 - u4*v5)*x2 - (u5*v4 - u4*v5)*x3)*y1 - (((v4 - v5)*x3 - v3*(x4 - x5) + v5*x4 - v4*x5)*u1 - (v5*x4 - v4*x5)*u3 - ((u4 - u5)*x3 - u3*(x4 - x5) + u5*x4 - u4*x5)*v1 + (u5*x4 - u4*x5)*v3 + ((u4 - u5)*v3 - u3*(v4 - v5) + u5*v4 - u4*v5)*x1 - (u5*v4 - u4*v5)*x3)*y2)

    z = (((w4*y4*z5 + (y5*z4 - y4*z5)*w3 - (w5*y5 + v4*z5 - v5*z5)*z4)*y3 + (v4*y5*z4 - (y5*z4 - y4*z5)*v3 - (w4*y5 - w5*y5 + v5*z5)*y4)*z3)*w1 - ((w4*y4*z5 + (y5*z4 - y4*z5)*w3 - (w5*y5 + v4*z5 - v5*z5)*z4)*y3 + (v4*y5*z4 - (y5*z4 - y4*z5)*v3 - (w4*y5 - w5*y5 + v5*z5)*y4)*z3)*w2 - ((w5*z4 - w4*z5)*w3*y3 + ((y5*z4 - y4*z5)*w3 - (w5*z4 - w4*z5)*y3 + (w5*y4 - w4*y5)*z3)*w1 - (w3*y3*(z4 - z5) + w4*y4*z5 + ((y4 - y5)*z3 - y3*(z4 - z5) + y5*z4 - y4*z5)*w1 - (w4*y4 - w5*y5 + v3*(z4 - z5) - v4*z4 + v5*z5)*z3 - (w5*y5 + v4*z5 - v5*z5)*z4)*w2 + (w4*y4*z5 - (w5*y5 + v4*z5 - v5*z5)*z4)*w3 - (w4*w5*y4 - v4*w5*z4 + (w5*z4 - w4*z5)*v3 - (w5*y5 - v5*z5)*w4)*z3)*y1 + ((w5*z4 - w4*z5)*w3*y3 - (w3*y3*(z4 - z5) + w4*y4*z5 - (w4*y4 - w5*y5 + v3*(z4 - z5) - v4*z4 + v5*z5)*z3 - (w5*y5 + v4*z5 - v5*z5)*z4)*w1 - (((y4 - y5)*z3 - y3*(z4 - z5) + y5*z4 - y4*z5)*w1 - (y5*z4 - y4*z5)*w3 + (w5*z4 - w4*z5)*y3 - (w5*y4 - w4*y5)*z3)*w2 + (w4*y4*z5 - (w5*y5 + v4*z5 - v5*z5)*z4)*w3 - (((w4 - w5)*z3 - w3*(z4 - z5) + w5*z4 - w4*z5)*w1 - ((w4 - w5)*z3 - w3*(z4 - z5) + w5*z4 - w4*z5)*w2)*y1 - (w4*w5*y4 - v4*w5*z4 + (w5*z4 - w4*z5)*v3 - (w5*y5 - v5*z5)*w4)*z3)*y2 - ((w5*y4 - w4*y5)*v3*z3 - ((y5*z4 - y4*z5)*w3 - (w5*z4 - w4*z5)*y3 + (w5*y4 - w4*y5)*z3)*v1 - (v3*(y4 - y5)*z3 + v4*y5*z4 - ((y4 - y5)*z3 - y3*(z4 - z5) + y5*z4 - y4*z5)*v1 - (w3*(y4 - y5) - w4*y4 + w5*y5 + v4*z4 - v5*z5)*y3 - (w4*y5 - w5*y5 + v5*z5)*y4)*w2 + (v4*y5*z4 - (w4*y5 - w5*y5 + v5*z5)*y4)*w3 - (w3*(w4 - w5)*y3 + w4*w5*y4 - v3*(w4 - w5)*z3 - v4*w5*z4 + ((w4 - w5)*z3 - w3*(z4 - z5) + w5*z4 - w4*z5)*v1 - ((w4 - w5)*y3 - w3*(y4 - y5) + w5*y4 - w4*y5)*w2 - (w4*y4 - w5*y5 - v4*z4 + v5*z5)*w3 - (w5*y5 - v5*z5)*w4)*y2 + (w4*w5*y4 - v4*w5*z4 - (w5*y4 - w4*y5)*w3 - (w5*y5 - v5*z5)*w4)*y3)*z1 + ((w5*y4 - w4*y5)*v3*z3 - ((y5*z4 - y4*z5)*w3 - (w5*z4 - w4*z5)*y3 + (w5*y4 - w4*y5)*z3)*v2 - (v3*(y4 - y5)*z3 + v4*y5*z4 - ((y4 - y5)*z3 - y3*(z4 - z5) + y5*z4 - y4*z5)*v2 - (w3*(y4 - y5) - w4*y4 + w5*y5 + v4*z4 - v5*z5)*y3 - (w4*y5 - w5*y5 + v5*z5)*y4)*w1 + (v4*y5*z4 - (w4*y5 - w5*y5 + v5*z5)*y4)*w3 - (w3*(w4 - w5)*y3 + w4*w5*y4 - v3*(w4 - w5)*z3 - v4*w5*z4 + ((w4 - w5)*z3 - w3*(z4 - z5) + w5*z4 - w4*z5)*v2 - ((w4 - w5)*y3 - w3*(y4 - y5) + w5*y4 - w4*y5)*w1 - (w4*y4 - w5*y5 - v4*z4 + v5*z5)*w3 - (w5*y5 - v5*z5)*w4)*y1 + (w4*w5*y4 - v4*w5*z4 - (w5*y4 - w4*y5)*w3 - (w5*y5 - v5*z5)*w4)*y3 - (((w4 - w5)*y3 - w3*(y4 - y5) + w5*y4 - w4*y5)*v1 - ((w4 - w5)*y3 - w3*(y4 - y5) + w5*y4 - w4*y5)*v2)*z1)*z2)/(((y5*z4 - y4*z5)*w3 - (w5*z4 - w4*z5)*y3 + (w5*y4 - w4*y5)*z3)*v1 - ((y5*z4 - y4*z5)*w3 - (w5*z4 - w4*z5)*y3 + (w5*y4 - w4*y5)*z3)*v2 + (((y4 - y5)*z3 - y3*(z4 - z5) + y5*z4 - y4*z5)*v2 - (y5*z4 - y4*z5)*v3 + (v5*z4 - v4*z5)*y3 - (v5*y4 - v4*y5)*z3)*w1 - (((y4 - y5)*z3 - y3*(z4 - z5) + y5*z4 - y4*z5)*v1 - (y5*z4 - y4*z5)*v3 + (v5*z4 - v4*z5)*y3 - (v5*y4 - v4*y5)*z3)*w2 - (((w4 - w5)*z3 - w3*(z4 - z5) + w5*z4 - w4*z5)*v2 - (w5*z4 - w4*z5)*v3 - ((v4 - v5)*z3 - v3*(z4 - z5) + v5*z4 - v4*z5)*w2 + (v5*z4 - v4*z5)*w3 - (v5*w4 - v4*w5)*z3)*y1 + (((w4 - w5)*z3 - w3*(z4 - z5) + w5*z4 - w4*z5)*v1 - (w5*z4 - w4*z5)*v3 - ((v4 - v5)*z3 - v3*(z4 - z5) + v5*z4 - v4*z5)*w1 + (v5*z4 - v4*z5)*w3 - (v5*w4 - v4*w5)*z3)*y2 + (((w4 - w5)*y3 - w3*(y4 - y5) + w5*y4 - w4*y5)*v2 - (w5*y4 - w4*y5)*v3 - ((v4 - v5)*y3 - v3*(y4 - y5) + v5*y4 - v4*y5)*w2 + (v5*y4 - v4*y5)*w3 + ((v4 - v5)*w3 - v3*(w4 - w5) + v5*w4 - v4*w5)*y2 - (v5*w4 - v4*w5)*y3)*z1 - (((w4 - w5)*y3 - w3*(y4 - y5) + w5*y4 - w4*y5)*v1 - (w5*y4 - w4*y5)*v3 - ((v4 - v5)*y3 - v3*(y4 - y5) + v5*y4 - v4*y5)*w1 + (v5*y4 - v4*y5)*w3 + ((v4 - v5)*w3 - v3*(w4 - w5) + v5*w4 - v4*w5)*y1 - (v5*w4 - v4*w5)*y3)*z2)

result2 = round . sum . solve
