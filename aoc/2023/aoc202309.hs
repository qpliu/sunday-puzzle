parse :: String -> [[Int]]
parse = map (map read . words) . lines

next :: [Int] -> Int
next ns
  | all (== 0) ns = 0
  | otherwise = last ns + next (zipWith (-) (drop 1 ns) ns)

result :: String -> Int
result = sum . map next . parse

testData :: String
testData = unlines [
    "0 3 6 9 12 15",
    "1 3 6 10 15 21",
    "10 13 16 21 30 45"
    ]

test :: ()
test
  | result testData /= 114 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/09.txt"

result2 :: String -> Int
result2 = sum . map (next . reverse) . parse

test2 :: ()
test2
  | result2 testData /= 2 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/09.txt"
