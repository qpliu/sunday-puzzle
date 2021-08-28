v = 22/3 :: Double

y0 = 50

tmax = 300/22 

dxdt s (x,y,t,_)= s*(v*t-x) / sqrt ((v*t-x)^2 + y^2)

dydt s (x,y,t,_) = -s*y / sqrt ((v*t-x)^2 + y^2)

step s dt p@(x,y,t,_) = (x + dt*dxdt s p, y + dt*dydt s p, t + dt, ((x-v*t)^2 + y^2,v*t))

time s dt = run (0,y0,0,(0,0))
  where
    run p@(x,y,t,_) | t > tmax = Nothing
                  | (x-v*t)^2 + y^2 < 0.1 = Just t
                  | otherwise = run (step s dt p)

path s dt = take (ceiling (tmax/dt)) (iterate (step s dt) (0,y0,0,(0,0)))

smax = 1.5*v
smin = v/2*sqrt 5

bsearch ds dt smin smax
  | smax - smin < ds = (s3,s3*15/v)
  | otherwise = maybe (bsearch ds dt s3 smax)
                      (const (bsearch ds dt smin s3))
                      (time s3 dt)
  where s3 = (smin+smax)/2

main = print $ bsearch 0.0001 0.0001 smin smax
