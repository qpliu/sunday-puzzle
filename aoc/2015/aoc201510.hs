import Data.List(group)

lookAndSay :: String -> String
lookAndSay s = concatMap say (group s)
  where
    say g@(c:_) = show (length g) ++ [c]

test :: ()
test
  | i 1 /= "11" = error "a"
  | i 2 /= "21" = error "b"
  | i 3 /= "1211" = error "c"
  | i 4 /= "111221" = error "d"
  | i 5 /= "312211" = error "e"
  | otherwise = ()
  where
    i n = head $ drop n $ iterate lookAndSay "1"

part1 :: String -> Int
part1 input = length $ head $ drop 40 $ iterate lookAndSay input

part2 :: String -> Int
part2 input = length $ head $ drop 50 $ iterate lookAndSay input
