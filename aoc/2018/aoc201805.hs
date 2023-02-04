import Data.Char(isLower,isSpace,toLower,toUpper)

react :: String -> String -> String
react chain [] = chain
react [] (c:rest) = react [c] rest
react (c1:r1) (c2:r2)
  | isLower c1 /= isLower c2 && toLower c1 == toLower c2 = react r1 r2
  | otherwise = react (c2:c1:r1) r2

test :: ()
test
  | react [] "aA" /= "" = error "a"
  | react [] "abBA" /= "" = error "b"
  | reverse (react [] "abAB") /= "abAB" = error "c"
  | reverse (react [] "aabAAB") /= "aabAAB" = error "d"
  | reverse (react [] "dabAcCaCBAcCcaDA") /= "dabCBAcaDA" = error "e"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . react [] . filter (not . isSpace)) $ readFile "input/05.txt"

improve :: String -> Char -> Int
improve s c = length $ react [] $ filter (/= (toLower c)) $ filter (/= (toUpper c)) s

searchImprove :: String -> Int
searchImprove s = minimum $ map (improve s) ['a'..'z']

test2 :: ()
test2
  | improve "dabAcCaCBAcCcaDA" 'a' /= 6 = error "a"
  | improve "dabAcCaCBAcCcaDA" 'b' /= 8 = error "b"
  | improve "dabAcCaCBAcCcaDA" 'c' /= 4 = error "c"
  | improve "dabAcCaCBAcCcaDA" 'd' /= 6 = error "d"
  | searchImprove "dabAcCaCBAcCcaDA" /= 4 = error "e"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (searchImprove . filter (not . isSpace)) $ readFile "input/05.txt"
