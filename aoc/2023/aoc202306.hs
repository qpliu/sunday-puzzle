import Data.Char(isDigit)

parse :: String -> [(Int,Int)]
parse = join . map (map read . drop 1 . words) . lines
  where join [time,distance] = zip time distance

-- Let t be the race time.  Let h be the hold time.
-- Let s be the speed.  s = h.
-- Let d be the distance.  d = s⋅(t-h) = th - h^2.
-- Given a distance, there are two hold times
--   h = (t ± √(t^2 - 4d))/2

ways :: (Int,Int) -> Int
ways (t,d)
  | (floor (sqrt (fromIntegral disc)))^2 == disc && even (t + floor (sqrt (fromIntegral disc))) = high - low - 1
  | otherwise = high - low + 1
  where
    disc = t^2 - 4*d
    high = floor ((fromIntegral t + sqrt (fromIntegral disc))/2)
    low = ceiling ((fromIntegral t - sqrt (fromIntegral disc))/2)

result :: String -> Int
result = product . map ways . parse

testData :: String
testData = unlines [
    "Time:      7  15   30",
    "Distance:  9  40  200"
    ]

test :: ()
test
  | map ways (parse testData) /= [4,8,9] = error "a"
  | result testData /= 288 = error "a"
  | otherwise = ()
  where

part1 :: IO Int
part1 = fmap result $ readFile "input/06.txt"

parse2 :: String -> (Int,Int)
parse2 = join . map (read . filter isDigit) . lines
  where join [time,distance] = (time,distance)

result2 :: String -> Int
result2 = ways . parse2

test2 :: ()
test2
  | result2 testData /= 71503 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/06.txt"
