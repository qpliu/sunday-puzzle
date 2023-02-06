import Data.Char(ord)

parse :: String -> [(Char,Char)]
parse = p . words
  where
    p ([a]:[b]:rest) = (a,b):p rest
    p _ = []

score :: (Char,Char) -> Int
score (a,b) = 1 + ord b - ord 'X' + outcome (ord a - ord 'A') (ord b - ord 'X')
  where
    outcome x y
      | x == y = 3
      | x == (y+2) `mod` 3 = 6
      | otherwise = 0

testData :: String
testData = unlines [
    "A Y",
    "B X",
    "C Z"
    ]

test :: ()
test
  | (sum . map score . parse) testData /= 15 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . map score . parse) $ readFile "input/02.txt"

score2 :: (Char,Char) -> Int
score2 (a,b)
  | b == 'X' = 1 + (opp+2) `mod` 3
  | b == 'Y' = 4 + opp `mod` 3
  | b == 'Z' = 7 + (opp+1) `mod` 3
  where
    opp = ord a - ord 'A'

test2 :: ()
test2
  | (sum . map score2 . parse) testData /= 12 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (sum . map score2 . parse) $ readFile "input/02.txt"
