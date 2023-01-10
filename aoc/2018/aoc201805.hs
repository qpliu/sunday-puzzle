{-
--- Day 5: Alchemical Reduction ---

You've managed to sneak in to the prototype suit manufacturing lab. The Elves
are making decent progress, but are still struggling with the suit's size
reduction capabilities.

While the very latest in 1518 alchemical technology might have solved their
problem eventually, you can do better. You scan the chemical composition of the
suit's material and discover that it is formed by extremely long polymers (one
of which is available as your puzzle input).

The polymer is formed by smaller units which, when triggered, react with each
other such that two adjacent units of the same type and opposite polarity are
destroyed. Units' types are represented by letters; units' polarity is
represented by capitalization. For instance, r and R are units with the same
type but opposite polarity, whereas r and s are entirely different types and do
not react.

For example:

 - In aA, a and A react, leaving nothing behind.
 - In abBA, bB destroys itself, leaving aA. As above, this then destroys
   itself, leaving nothing.
 - In abAB, no two adjacent units are of the same type, and so nothing happens.
 - In aabAAB, even though aa and AA are of the same type, their polarities
   match, and so nothing happens.

Now, consider a larger example, dabAcCaCBAcCcaDA:

| dabAcCaCBAcCcaDA  The first 'cC' is removed.
| dabAaCBAcCcaDA    This creates 'Aa', which is removed.
| dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
| dabCBAcaDA        No further actions can be taken.

After all possible reactions, the resulting polymer contains 10 units.

How many units remain after fully reacting the polymer you scanned? (Note: in
this puzzle and others, the input is large; if you copy/paste your input, make
sure you get the whole thing.)
-}

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
