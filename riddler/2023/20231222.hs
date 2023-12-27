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

collatzSeries :: Integer -> [Integer]
collatzSeries = takeWhile (/= 1) . iterate collatz

inverseCollatz :: Integer -> [Integer]
inverseCollatz n
  | (n-1) `mod` 3 == 0 && odd ((n-1) `div` 3) = [(n-1) `div` 3,2*n]
  | otherwise = [2*n]

sources :: [Integer] -> [Integer]
sources = sort . concatMap inverseCollatz . filter ((/= 0) . (`mod` 3))

data T a = M a | T a (T a) (T a) | Z deriving Show

instance Foldable T where
  foldr f b t = foldr f b (flatten t)

tree :: Int -> Integer -> T Integer
tree maxDepth n
  | maxDepth <= 0 = Z
  | otherwise =
      case n `mod` 6 of
        0 -> M (n `div` 2)
        3 -> M n
        4 -> T (n `div` 3) (tree (maxDepth-1) (2*n)) (tree (maxDepth-1) (n `div` 3))
        _ -> tree (maxDepth-1) (2*n)

flatten :: T a -> [a]
flatten t = f [] t
  where
    f l (M n) = n:l
    f l (T n t1 t2) = f (f (n:l) t1) t2
    f l Z = l
