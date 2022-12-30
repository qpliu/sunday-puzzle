{-
--- Day 3: Squares With Three Sides ---

Now that you can think clearly, you move deeper into the labyrinth of hallways
and office furniture that makes up this part of Easter Bunny HQ. This must be a
graphic design department; the walls are covered in specifications for
triangles.

Or are they?

The design document gives the side lengths of each triangle it describes,
but... 5 10 25? Some of these aren't triangles. You can't help but mark the
impossible ones.

In a valid triangle, the sum of any two sides must be larger than the remaining
side. For example, the "triangle" given above is impossible, because 5 + 10 is
not larger than 25.

In your puzzle input, how many of the listed triangles are possible?
-}

import Data.List(sort)

possible :: String -> Bool
possible s = ok $ reverse $ sort $ map read $ words s
  where
    ok sides = sum (take 1 sides) < sum (drop 1 sides)

test :: ()
test
  | possible "5 10 25" = error "a"
  | not (possible "1 1 1") = error "b"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . filter possible . lines) $ readFile "input/03.txt"

triangles :: [[String]] -> [[String]]
triangles lines
  | null lines = []
  | otherwise = transpose (take 3 lines) ++ triangles (drop 3 lines)
  where
    transpose ls = [map head ls,map (head . tail) ls,map (head . drop 2) ls]

test2 :: ()
test2
  | triangles (map words (lines testData)) /= [["101","102","103"],["301","302","303"],["501","502","503"],["201","202","203"],["401","402","403"],["601","602","603"]] = error "a"
  | otherwise = ()
  where
    testData = "101 301 501\n102 302 502\n103 303 503\n201 401 601\n202 402 602\n203 403 603"

possible2 :: [String] -> Bool
possible2 line = ok $ reverse $ sort $ map read line
  where
    ok sides = sum (take 1 sides) < sum (drop 1 sides)

part2 :: IO Int
part2 = fmap (length . filter possible2 . triangles . map words . lines) $ readFile "input/03.txt"
