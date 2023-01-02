{-
--- Day 2: Corruption Checksum ---

As you walk through the door, a glowing humanoid shape yells in your direction.
"You there! Your state appears to be idle. Come help us repair the corruption
in this spreadsheet - if we take another millisecond, we'll have to display an
hourglass cursor!"

The spreadsheet consists of rows of apparently-random numbers. To make sure the
recovery process is on the right track, they need you to calculate the
spreadsheet's checksum. For each row, determine the difference between the
largest value and the smallest value; the checksum is the sum of all of these
differences.

For example, given the following spreadsheet:

| 5 1 9 5
| 7 5 3
| 2 4 6 8

 - The first row's largest and smallest values are 9 and 1, and their
   difference is 8.
 - The second row's largest and smallest values are 7 and 3, and their
   difference is 4.
 - The third row's difference is 6.

In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.

What is the checksum for the spreadsheet in your puzzle input?
-}

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
