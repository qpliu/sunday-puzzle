flipThreshold :: Int -> Double
flipThreshold n = (2.0 - 2.0**(1.0+1.0/(fromIntegral (n - 1)))) / (1.0 - 2.0**(1.0+1.0/(fromIntegral (n - 1))))

chanceOfRelease :: Int -> Double
chanceOfRelease n = (1.0 - 0.5*flipThreshold n)**(fromIntegral n) - (1.0 - flipThreshold n)**(fromIntegral n)
