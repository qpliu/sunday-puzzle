import Data.Set(Set,empty,fromList,insert,member)

scan :: [Int] -> [Int]
scan = fst . foldr collect ([],empty)
  where
    collect :: Int -> ([Int],Set Int) -> ([Int],Set Int)
    collect n (results,products)
      | (n*(2020-n)) `member` products = (n*(2020-n):results,products)
      | otherwise = (results,insert (n*(2020-n)) products)

test :: ()
test
  | scan [1721, 979, 366, 299, 675, 1456] /= [514579] = error "a"
  | otherwise = ()

part1 :: IO [Int]
part1 = fmap (scan . map read . words) $ readFile "input/01.txt"

scan2 :: [Int] -> [Int]
scan2 nums = [a*b*(2020-a-b) | a <- nums, b <- nums, b >= a, (2020-a-b) >= b , (2020-a-b) `member` set]
  where
    set = fromList nums

test2 :: ()
test2
  | scan2 [1721, 979, 366, 299, 675, 1456] /= [241861950] = error "a"
  | otherwise = ()

part2 :: IO [Int]
part2 = fmap (scan2 . map read . words) $ readFile "input/01.txt"
