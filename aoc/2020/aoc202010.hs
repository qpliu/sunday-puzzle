import Data.List(group,sort)

distribution :: [Int] -> [(Int,Int)]
distribution = map toCounts . group . sort . diffs . sort
  where
    diffs as@(a:_) = 3 : zipWith (-) as (0:as)
    toCounts list@(i:_) = (i,length list)

counts :: [Int] -> [(Int,Int)] -> [Int]
counts diffs distr = map (maybe 0 id . (`lookup` distr)) diffs

testData :: [[Int]]
testData = [[16,10,15,5,1,11,7,19,6,12,4],[28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]]

test :: ()
test
  | (counts [1,3] . distribution) (testData !! 0) /= [7,5] = error "a"
  | (counts [1,3] . distribution) (testData !! 1) /= [22,10] = error "b"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (product . counts [1,3] . distribution . map read . words) $ readFile "input/10.txt"

count :: [Int] -> Int
count [] = 1
count [_] = 1
count as = slowCount (head as) (map fst bs) * count (map fst cs)
  where
    (bs,cs) = span ((< 3) . snd) $ zip (tail as) $ zipWith (-) (tail as) as

slowCount :: Int -> [Int] -> Int
slowCount _ [] = 1
slowCount level [a]
  | level+3 >= a = 1
  | otherwise = 0
slowCount level (a:as)
  | level+3 < a = 0
  | otherwise = slowCount level as + slowCount a as

test2 :: ()
test2
  | (count . (0:) . sort) (testData !! 0) /= 8 = error "a"
  | (count . (0:) . sort) (testData !! 1) /= 19208 = error "b"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (count . (0:) . sort . map read . words) $ readFile "input/10.txt"
