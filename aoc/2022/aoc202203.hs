import Data.Char(ord)
import Data.Set(Set,elems,fromList,intersection)

parse :: String -> [(String,String)]
parse = map toCompartments . words
  where
    toCompartments s = splitAt (length s `div` 2) s

getMispacked :: (String,String) -> String
getMispacked (a,b) = elems $ intersection (fromList a) (fromList b)

getPriority :: Char -> Int
getPriority ch
  | ch >= 'a' && ch <= 'z' = ord ch + 1 - ord 'a'
  | ch >= 'A' && ch <= 'Z' = ord ch + 27 - ord 'A'
  | otherwise = 0

testData :: String
testData = unlines [
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
    ]

test :: ()
test
  | (sum . concatMap (map getPriority . getMispacked) . parse) testData /= 157 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . concatMap (map getPriority . getMispacked) . parse) $ readFile "input/03.txt"

parse2 :: String -> [(String,String,String)]
parse2 = p . words
  where
    p (a:b:c:rest) = (a,b,c):p rest
    p _ = []

getBadge :: (String,String,String) -> Char
getBadge (a,b,c) = minimum (intersection (fromList a) (intersection (fromList b) (fromList c)))

test2 :: ()
test2
  | (sum . map (getPriority . getBadge) . parse2) testData /= 70 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (sum . map (getPriority . getBadge) . parse2) $ readFile "input/03.txt"
