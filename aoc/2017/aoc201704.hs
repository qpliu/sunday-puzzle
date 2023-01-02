{-
--- Day 4: High-Entropy Passphrases ---

A new system policy has been put in place that requires all accounts to use a
passphrase instead of simply a password. A passphrase consists of a series of
words (lowercase letters) separated by spaces.

To ensure security, a valid passphrase must contain no duplicate words.

For example:

 - aa bb cc dd ee is valid.
 - aa bb cc dd aa is not valid - the word aa appears more than once.
 - aa bb cc dd aaa is valid - aa and aaa count as different words.

The system's full passphrase list is available as your puzzle input. How many
passphrases are valid?
-}

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
