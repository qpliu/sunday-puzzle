nsquare :: Double -> Int
nsquare k = floor (k/2)*4

nhexagon :: Double -> Int
nhexagon k
  | k < 2 = 0
  | odd n = 7*(1 + n `div` 2)
  | otherwise = 4 + 7*(n `div` 2)
  where
    n = floor ((k - 2)/sqrt 3)

main :: IO ()
main = print $ maximum $ [k | k <- [1..500], nsquare k >= nhexagon k]
