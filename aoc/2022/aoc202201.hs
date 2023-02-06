import Data.List(sort)

parse :: String -> [[Int]]
parse = p . lines
  where
    p items | null items = [] | otherwise = map read this : p (drop 1 rest)
      where
        (this,rest) = span (/= "") items

testData :: String
testData = unlines [
    "1000",
    "2000",
    "3000",
    "",
    "4000",
    "",
    "5000",
    "6000",
    "",
    "7000",
    "8000",
    "9000",
    "",
    "10000"
    ]

test :: ()
test
  | (maximum . map sum . parse) testData /= 24000 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (maximum . map sum . parse) $ readFile "input/01.txt"

test2 :: ()
test2
  | (sum . take 3 . reverse . sort . map sum . parse) testData /= 45000 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (sum . take 3 . reverse . sort . map sum . parse) $ readFile "input/01.txt"
