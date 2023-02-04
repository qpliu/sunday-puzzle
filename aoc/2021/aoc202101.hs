countLarger :: [Int] -> Int
countLarger ms = length $ filter (uncurry (>)) $ zip (drop 1 ms) ms

test :: ()
test
  | countLarger [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] /= 7 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (countLarger . map read . words) $ readFile "input/01.txt"

slidingWindow :: [Int] -> [Int]
slidingWindow ms = zipWith3 (\ a b c -> a+b+c) ms (drop 1 ms) (drop 2 ms)

test2 :: ()
test2
  | (countLarger . slidingWindow)  [199, 200, 208, 210, 200, 207, 240, 269, 260, 263] /= 5 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (countLarger . slidingWindow . map read . words) $ readFile "input/01.txt"
