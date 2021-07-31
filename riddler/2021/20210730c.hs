
x :: Int -> Double -> Double -> Double
x n f theta = sum $ take n $ [f^i*cos(fromIntegral i*theta) | i <- [0..]]

y :: Int -> Double -> Double -> Double
y n f theta = sum $ take n $ [f^i*sin(fromIntegral i*theta) | i <- [1..]]

table :: Int -> Double -> Double -> [(Double,Double)]
table n f dtheta = [(x n f theta,y n f theta) | theta <- [0,dtheta..2*pi]]

p :: Int -> Double -> Double -> IO ()
p n f dtheta =
  mapM_ (\(x,y) -> putStrLn (show x++","++show y)) $ table n f dtheta
