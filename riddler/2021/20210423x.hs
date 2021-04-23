twocut :: Double -> Double
twocut a = (1 - (b-b/a)/(a-1/a))^2 + (1 - (b-b/a)/(a-1/a))*((b-b/a)/(a-1/a) - (b-1)/a)
  where b = sqrt (1+a^2)

maxtwocut delta = maximum [(twocut a,a) | a <- [0.1,0.1+delta..0.9]]

onecut :: Double
onecut = (2-sqrt 2)^2/2

atwocut :: Double -> Double -> Double
atwocut a c = rectArea + leftTriArea + lowerTriArea
  where
    b = sqrt (1+a^2)
    d = sqrt (1+c^2)
    crossx = (b-d)/(a-c)
    crossy = (a*d-b*c)/(a-c)
    upperx = min (b/a - 1/a) (d/c - 1/c)
    righty = min (-a+b) (-c+d)
    rectArea = (1-crossx)*(1-crossy)
    leftTriArea = (crossx - upperx)*(1 - crossy)/2
    lowerTriArea = (1 - crossx)*(crossy - righty)/2

maxatwocut delta = maximum [(atwocut a c,a,c) | a <- [0.1,0.1+delta..0.9], c <- [a+delta,a+2*delta..10]]

main = do
    print (maxtwocut 0.00001)
    print (4 - 3*onecut - fst (maxtwocut 0.00001))
    print (maxatwocut 0.01)
