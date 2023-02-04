import Data.Set(Set,empty,insert,member)

parse :: String -> [Int]
parse = map (read . filter f) . words
  where f char = char /= '+' && char /= ','

test :: ()
test
  | sum (parse "+1, -2, +3, +1") /= 3 = error "a"
  | sum (parse "+1, +1, +1") /= 3 = error "b"
  | sum (parse "+1, +1, -2") /= 0 = error "c"
  | sum (parse "-1, -2, -3") /= -6 = error "d"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . parse) $ readFile "input/01.txt"

search :: Set Int -> Int -> [Int] -> Int
search set total (n:ns)
  | member total set = total
  | otherwise = search (insert total set) (total+n) ns

searchRepeating :: [Int] -> Int
searchRepeating ns = search empty 0 $ concat $ repeat ns

test2 :: ()
test2
  | searchRepeating [1,-1] /= 0 = error "a"
  | searchRepeating [3,3,4,-2,-4] /= 10 = error "b"
  | searchRepeating [-6,3,8,5,-6] /= 5 = error "c"
  | searchRepeating [7,7,2,-7,-4] /= 14 = error "d"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (searchRepeating . parse) $ readFile "input/01.txt"
