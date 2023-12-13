import Data.List(groupBy,nub)

parse :: String -> [[String]]
parse = filter (/= [""]) . groupBy (\ a b -> a /= "" && b /= "") . lines

-- rotate by 90 degrees
rotate :: [[a]] -> [[a]]
rotate pattern = foldr (zipWith (:)) (map (const []) (head pattern)) pattern

findReflections :: Eq a => [a] -> [Int]
findReflections pattern = search initI (drop initI pattern) (reverse $ drop initI pattern)
  where
    (initI,initPat,initRPat)
      | odd (length pattern) = (1,tail pattern,reverse (tail pattern))
      | otherwise = (0,pattern,reverse pattern)
    search i pat rpat
      | null pat = search2 (reverse $ drop initI $ reverse pattern) (drop initI $ reverse pattern)
      | pat == rpat = (length pat `div` 2 + i) : search (i+2) (tail $ tail pat) (init $ init rpat)
      | otherwise = search (i+2) (tail $ tail pat) (init $ init rpat)
    search2 pat rpat
      | null pat = []
      | pat == rpat = (length pat `div` 2) : search2 (init $ init pat) (tail $ tail rpat)
      | otherwise = search2 (init $ init pat) (tail $ tail rpat)

summarizeReflection :: Eq a => [[a]] -> [Int]
summarizeReflection pattern = map (100*) (findReflections pattern) ++ (findReflections $ rotate pattern)

result :: String -> Int
result = sum . concatMap summarizeReflection . parse

testData :: String
testData = unlines [
    "#.##..##.",
    "..#.##.#.",
    "##......#",
    "##......#",
    "..#.##.#.",
    "..##..##.",
    "#.#.##.#.",
    "",
    "#...##..#",
    "#....#..#",
    "..##..###",
    "#####.##.",
    "#####.##.",
    "..##..###",
    "#....#..#"
    ]

test :: ()
test
  | result testData /= 405 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/13.txt"

result2 :: String -> Int
result2 = sum . concatMap fixSmudge . parse

test2 :: ()
test2
  | result2 testData /= 400 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/13.txt"

fixSmudge :: [String] -> [Int]
fixSmudge pattern = filter (not . (`elem` oldReflections)) $ nub $ concat [summarizeReflection (lines $ desmudge i) | i <- [0 .. length pat - 1]]
  where
    oldReflections = summarizeReflection pattern
    pat = unlines pattern
    desmudge i = front ++ desmudgeChar back
      where (front,back) = splitAt i pat
    desmudgeChar ('.':rest) = '#':rest
    desmudgeChar ('#':rest) = '.':rest
    desmudgeChar rest = rest
