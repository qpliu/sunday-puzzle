parse :: String -> [[Int]]
parse = map (map read . words) . lines

checksum :: [[Int]] -> Int
checksum = sum . map checkrow
  where checkrow row = maximum row - minimum row

test :: ()
test
  | checksum testData /= 18 = error "a"
  | otherwise = ()
  where testData = parse "5 1 9 5\n7 5 3\n2 4 6 8"

part1 :: IO Int
part1 = fmap (checksum . parse) $ readFile "input/02.txt"

checksum2 :: [[Int]] -> Int
checksum2 = sum . map checkrow
  where checkrow row = head [a `div` b | a <- row, b <- row, a /= b, a `mod` b == 0]

test2 :: ()
test2
  | checksum2 (parse testData) /= 9 = error "a"
  | otherwise = ()
  where testData = "5 9 2 8\n9 4 7 3\n3 8 6 5"

part2 :: IO Int
part2 = fmap (checksum2 . parse) $ readFile "input/02.txt"
