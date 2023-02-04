fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

test :: ()
test
  | fuel 12 /= 2 = error "a"
  | fuel 14 /= 2 = error "b"
  | fuel 1969 /= 654 = error "c"
  | fuel 100756 /= 33583 = error "d"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . map (fuel . read) . words) $ readFile "input/01.txt"

-- This gives the wrong answer due to integer division and multiplication
-- not being inverses.
totalFuel :: Int -> Int
totalFuel mass = max 0 (mass `div` 2 - 3)

totalFuel2 :: Int -> Int
totalFuel2 = sum . tail . takeWhile (> 0) . iterate fuel

test2 :: ()
test2
  | totalFuel2 14 /= 2 = error "a"
  | totalFuel2 1969 /= 966 = error "b"
  | totalFuel2 100756 /= 50346 = error "c"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (sum . map (totalFuel2 . read) . words) $ readFile "input/01.txt"
