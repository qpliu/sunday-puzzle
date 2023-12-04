import Data.Char(isDigit)
import Data.Set(Set,fromList,intersection,size)

parse :: String -> [(Int,([Int],[Int]))]
parse = parseCard . words
  where
    parseCard [] = []
    parseCard ("Card":n:rest) = (read (init n),(map read win,map read have)) : parseCard rest3
      where
        (win,rest2) = span (/= "|") rest
        (have,rest3) = span (/= "Card") (drop 1 rest2)

wins :: (Int,([Int],[Int])) -> Int
wins (_,(win,have)) = size $ intersection (fromList have) (fromList win)

score :: Int -> Int
score n | n <= 0 = 0 | otherwise = 2^(n-1)

result :: String -> Int
result = sum . map (score . wins) . parse

testData :: String
testData = unlines [
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
    ]

test :: ()
test
  | result testData /= 13 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/04.txt"

counts :: [(Int,([Int],[Int]))] -> [Int]
counts = copies . map (flip (,) 1 . wins)
  where
    copies [] = []
    copies ((win,count):rest) = count : copies (map (fmap (count+)) rest1 ++ rest2)
      where (rest1,rest2) = splitAt win rest

result2 :: String -> Int
result2 = sum . counts . parse

test2 :: ()
test2
  | result2 testData /= 30 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/04.txt"
