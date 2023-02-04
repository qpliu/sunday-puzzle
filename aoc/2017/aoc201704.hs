import Data.List(sort)
import Data.Set(fromList,size)

valid :: [String] -> Bool
valid phrase = length phrase == size (fromList phrase)

test :: ()
test
  | not $ valid $ words "aa bb cc dd ee" = error "a"
  | valid $ words "aa bb cc dd aa" = error "b"
  | not $ valid $ words "aa bb cc dd aaa" = error "c"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . filter valid . map words . lines) $ readFile "input/04.txt"

valid2 :: [String] -> Bool
valid2 phrase = length phrase == size (fromList (map sort phrase))

test2 :: ()
test2
  | not $ valid2 $ words "abcde fghij" = error "a"
  | valid2 $ words "abcde xyz ecdab" = error "a"
  | not $ valid2 $ words "a ab abc abd abf abj" = error "a"
  | not $ valid2 $ words "iiii oiii ooii oooi oooo" = error "a"
  | valid2 $ words "oiii ioii iioi iiio" = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (length . filter valid2 . map words . lines) $ readFile "input/04.txt"
