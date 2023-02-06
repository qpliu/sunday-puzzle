import Data.List(nub)

detect :: Int -> Int -> String -> Int
detect ndistinct n str
  | null str = error "no start"
  | (length . nub . take ndistinct) str /= ndistinct = detect ndistinct (n+1) (drop 1 str)
  | otherwise = n+ndistinct

test :: ()
test
  | detect 4 0 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" /= 7 = error "a"
  | detect 4 0 "bvwbjplbgvbhsrlpgdmjqwftvncz" /= 5 = error "b"
  | detect 4 0 "nppdvjthqldpwncqszvftbrmjlhg" /= 6 = error "c"
  | detect 4 0 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" /= 10 = error "d"
  | detect 4 0 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" /= 11 = error "e"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (detect 4 0) $ readFile "input/06.txt"

test2 :: ()
test2
  | detect 14 0 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" /= 19 = error "a"
  | detect 14 0 "bvwbjplbgvbhsrlpgdmjqwftvncz" /= 23 = error "b"
  | detect 14 0 "nppdvjthqldpwncqszvftbrmjlhg" /= 23 = error "c"
  | detect 14 0 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" /= 29 = error "d"
  | detect 14 0 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" /= 26 = error "e"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (detect 14 0) $ readFile "input/06.txt"
