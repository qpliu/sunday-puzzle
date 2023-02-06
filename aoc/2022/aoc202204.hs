parse :: String -> [((Int,Int),(Int,Int))]
parse = map parsePair . lines
  where
    parsePair str =
        let (a,b) = span (/= ',') str in (parseRange a,parseRange (drop 1 b))
    parseRange str = let (a,b) = span (/= '-') str in (read a,read (drop 1 b))

fullyContained :: ((Int,Int),(Int,Int)) -> Bool
fullyContained ((a,b),(c,d)) = (c >= a && d <= b) || (a >= c && b <= d)

testData :: String
testData = unlines [
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8"
    ]

test :: ()
test
  | (length . filter fullyContained . parse) testData /= 2 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . filter fullyContained . parse) $ readFile "input/04.txt"

overlaps :: ((Int,Int),(Int,Int)) -> Bool
overlaps ((a,b),(c,d)) = not (c > b || a > d)

test2 :: ()
test2
  | (length . filter overlaps . parse) testData /= 4 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (length . filter overlaps . parse) $ readFile "input/04.txt"
