import Data.Char(isDigit)

parse :: String -> [(Int,Int)]
parse = map (parse1 . words) . lines
  where parse1 [depth,range] = (read $ filter isDigit depth,read range)

scannerPosition :: (Int,Int) -> Int -> Int
scannerPosition (_,range) t
  | t `mod` (2*range-2) < range = t `mod` (2*range-2)
  | otherwise = 2*range-2 - t `mod` (2*range-2)

severity :: (Int,Int) -> Int -> Int
severity scanner@(depth,range) t
  | scannerPosition scanner t == 0 = depth*range
  | otherwise = 0

severity1 :: (Int,Int) -> Int
severity1 scanner@(depth,range) = severity scanner depth

testData :: String
testData = "0: 3\n1: 2\n4: 4\n6: 4"

test :: ()
test
  | map severity1 (parse testData) /= [0,0,0,24] = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . map severity1 . parse) $ readFile "input/13.txt"

passes :: [(Int,Int)] -> Int -> Bool
passes scanners delay = not $ any caughtBy scanners
  where
    caughtBy scanner@(depth,_) = scannerPosition scanner (depth+delay) == 0

test2 :: ()
test2
  | head (filter (passes (parse testData)) [0..]) /= 10 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (head . flip filter [0..] . passes . parse) $ readFile "input/13.txt"
