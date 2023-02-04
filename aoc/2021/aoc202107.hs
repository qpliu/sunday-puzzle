-- fuel = sum abs(p - c[i])

parse :: String -> [Int]
parse str = read ("["++str++"]")

fuel :: [Int] -> Int -> Int
fuel crabs pos = sum $ map (abs . (pos -)) crabs

minFuel :: [Int] -> Int
minFuel crabs = minimum $ map (fuel crabs) [0..length crabs]

test :: ()
test
  | (minFuel . parse) "16,1,2,0,4,2,7,1,2,14" /= 37 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (minFuel . parse) $ readFile "input/07.txt"

fuel2 :: [Int] -> Int -> Int
fuel2 crabs pos = sum $ map (gauss . abs . (pos -)) crabs
  where gauss n = n*(n+1) `div` 2

minFuel2 :: [Int] -> Int
minFuel2 crabs = minimum $ map (fuel2 crabs) [0..length crabs]

test2 :: ()
test2
  | (minFuel2 . parse) "16,1,2,0,4,2,7,1,2,14" /= 168 = error "a"
  | (flip fuel2 2 . parse) "16,1,2,0,4,2,7,1,2,14" /= 206 = error "b"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (minFuel2 . parse) $ readFile "input/07.txt"
