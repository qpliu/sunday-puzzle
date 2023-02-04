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
