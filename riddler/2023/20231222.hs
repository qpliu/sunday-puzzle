import Data.List(sort)

prisms :: [(Integer,(Integer,Integer,Integer))]
prisms = sort [(l*m*root n2,(l,m,root n2)) | l <- [1..2024], m <- [l..2024], n2 <- [2024^2 - l^2 - m^2], n2 >= m^2, isSquare n2]
  where
    squares = map (^2) [1..2024]
    isSquare n = n == root n^2
    root n = floor (sqrt (fromIntegral n))

collatz :: Integer -> Integer
collatz n
  | odd n = 3*n + 1
  | otherwise = n `div` 2

inverseCollatz :: Integer -> [Integer]
inverseCollatz n
  | (n-1) `mod` 3 == 0 && odd ((n-1) `div` 3) = [(n-1) `div` 3,2*n]
  | otherwise = [2*n]

sources :: [Integer] -> [Integer]
sources = sort . concatMap inverseCollatz . filter ((/= 0) . (`mod` 3))
