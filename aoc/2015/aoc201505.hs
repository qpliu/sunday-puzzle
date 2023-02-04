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
