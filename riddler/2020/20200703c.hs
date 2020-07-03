both :: (Eq a, Ord a) => [a] -> [a] -> [a]
both (a:as) (b:bs)
  | a == b = a : both as bs
  | a > b = both (a:as) bs
  | otherwise = both as (b:bs)

twiceSquares :: [Integer]
twiceSquares = [2*n^2 | n <- [1..]]

centeredPentagonal :: [Integer]
centeredPentagonal = [(5*n^2 + 5*n + 2) `div` 2 | n <- [1..]]

main :: IO ()
main = print (take 5 (both twiceSquares (map (flip (-) 1) centeredPentagonal)))
