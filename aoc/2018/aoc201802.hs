import Data.List(group,sort)

checksum :: [String] -> Int
checksum boxIDs = length (filter (elem 2) counts) * length (filter (elem 3) counts)
  where counts = map (map length . group . sort) boxIDs

testData :: String
testData = "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab"

test :: ()
test
  | checksum (words testData) /= 12 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (checksum . words) $ readFile "input/02.txt"

correctPair :: String -> String -> Maybe String
correctPair a b
  | length c == length a - 1 = Just c
  | otherwise = Nothing
  where
    c = map fst $ filter (uncurry (==)) $ zip a b

search :: [String] -> [String]
search boxIDs = [let Just d = c in d | a <- boxIDs, b <- boxIDs, c <- [correctPair a b], c /= Nothing]

testData2 :: String
testData2 = "abcde\nfghij\nklmno\npqrst\nfguij\naxcye\nwvxyz"

test2 :: ()
test2
  | search (words testData2) /= ["fgij","fgij"] = error "a"
  | otherwise = ()

part2 :: IO [String]
part2 = fmap (search . words) $ readFile "input/02.txt"
