f :: Int -> Double
f nsides = (n+1)*tan(pi/(n+1))/(n*tan(pi/n))
  where n = fromIntegral nsides

kmin :: Int -> Double
kmin nsides = (g*n + 1 - sqrt(1-g))/(g*n*n + 2*n + 1)
  where n = fromIntegral nsides
        g = 1 - f nsides

main :: IO ()
main = mapM_ print [(n+1,kmin n) | n <- [3..24]]
