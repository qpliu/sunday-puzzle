import Data.Set(Set,member,fromList)

parse :: String -> [(Int,Int)]
parse = map fst . filter ((== '#') . snd) . concatMap parseRow . zip [0..] . lines
  where
    parseRow (row,line) = zip (map (flip (,) row) [0..]) line

emptyRows :: [(Int,Int)] -> [Int]
emptyRows galaxies = filter (not . (`member` rows)) [0..maximum rows]
  where rows = fromList $ map snd galaxies

emptyCols :: [(Int,Int)] -> [Int]
emptyCols galaxies = filter (not . (`member` cols)) [0..maximum cols]
  where cols = fromList $ map fst galaxies

expand :: Int -> [(Int,Int)] -> [(Int,Int)]
expand factor galaxies = map toExpanded galaxies
  where
    rows = emptyRows galaxies
    cols = emptyCols galaxies
    toExpanded (x,y) =
        (x + factor*length (filter (< x) cols),y + factor*length (filter (< y) rows))

distances :: [(Int,Int)] -> Int
distances galaxies = sum [dist a b | a <- galaxies, b <- galaxies, b < a]
  where
    dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

result :: String -> Int
result = distances . expand 1 . parse

testData :: String
testData = unlines [
    "...#......",
    ".......#..",
    "#.........",
    "..........",
    "......#...",
    ".#........",
    ".........#",
    "..........",
    ".......#..",
    "#...#....."
    ]

test :: ()
test
  | result testData /= 374 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/11.txt"

result2 :: String -> Int
result2 = distances . expand 999999 . parse

test2 :: ()
test2
  | (distances . expand 9 . parse) testData /= 1030 = error "a"
  | (distances . expand 99 . parse) testData /= 8410 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/11.txt"
