{-
--- Day 5: Doesn't He Have Intern-Elves For This? ---

Santa needs help figuring out which strings in his text file are naughty or
nice.

A nice string is one with all of the following properties:

 - It contains at least three vowels (aeiou only), like aei, xazegov, or
   aeiouaeiouaeiou.
 - It contains at least one letter that appears twice in a row, like xx, abcdde
   (dd), or aabbccdd (aa, bb, cc, or dd).
 - It does not contain the strings ab, cd, pq, or xy, even if they are part of
   one of the other requirements.

For example:

 - ugknbfddgicrmopn is nice because it has at least three vowels
   (u...i...o...), a double letter (...dd...), and none of the disallowed
   substrings.
 - aaa is nice because it has at least three vowels and a double letter, even
   though the letters used by different rules overlap.
 - jchzalrnumimnmhp is naughty because it has no double letter.
 - haegwjzuvuyypxyu is naughty because it contains the string xy.
 - dvszwmarrgswjxmb is naughty because it contains only one vowel.

How many strings are nice?
-}
import Data.Set(empty,insert,member)

nice :: String -> Bool
nice s = n (0,False) s
  where
    n (vowels,double) [] = vowels >= 3 && double
    n (vowels,double) [l] = vowels >= (if vowel l then 2 else 3) && double
    n (vowels,double) (l:s@(m:_))
      | [l,m] `elem` ["ab","cd","pq","xy"] = False
      | otherwise = n (vowels + if vowel l then 1 else 0,double || l == m) s
    vowel l = l `elem` "aeiou"

main :: IO ()
main = getContents >>= print . length . filter nice . words

test :: ()
test
  | not $ nice "ugknbfddgicrmopn" = error "a"
  | not $ nice "aaa" = error "b"
  | nice "jchzalrnumimnmhp" = error "c"
  | nice "haegwjzuvuyypxyu" = error "d"
  | nice "dvszwmarrgswjxmb" = error "e"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . filter nice . words) (readFile "input/05.txt")

part2nice :: String -> Bool
part2nice s = hasDuplicatePair s empty && hasSandwich s
  where
    hasSandwich (a:as@(b:c:_)) = a == c || hasSandwich as
    hasSandwich _ = False
    hasDuplicatePair (a:as@(b:bs@(c:_))) set
      | (a,b) `member` set = True
      | a == b && b == c = hasDuplicatePair bs (insert (a,b) set)
      | otherwise = hasDuplicatePair as (insert (a,b) set)
    hasDuplicatePair [a,b] set = (a,b) `member` set

part2 :: IO Int
part2 = fmap (length . filter part2nice . words) (readFile "input/05.txt")
