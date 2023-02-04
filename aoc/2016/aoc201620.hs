import Data.Char(isDigit)

parse :: String -> (Integer,Integer)
parse s = (read (takeWhile isDigit s),read (dropWhile (not . isDigit) $ dropWhile isDigit s))

combine :: [(Integer,Integer)] -> String -> [(Integer,Integer)]
combine blacklist s = addTo (parse s) blacklist
  where
    addTo (a,b) [] = [(a,b)]
    addTo (a,b) bl@((x,y):rest)
      | b+1 < x = (a,b):bl
      | b <= y = (min a x,y):rest
      | a <= y+1 = addTo (min a x,b) rest
      | otherwise = (x,y):addTo (a,b) rest

getList :: String -> [(Integer,Integer)]
getList s = foldl combine [] $ words s

lowest :: [(Integer,Integer)] -> Integer
lowest [] = error "bad input"
lowest ((a,b):_) | a /= 0 = 0 | otherwise = b+1

test :: ()
test
  | lowest (getList "5-8\n0-2\n4-7") /= 3 = error "a"
  | otherwise = ()

part1 :: IO Integer
part1 = fmap (lowest . getList) $ readFile "input/20.txt"

count :: Integer -> Integer -> [(Integer,Integer)] -> Integer
count runningTotal endOfLastRange [] =
    runningTotal + 4294967295 - endOfLastRange
count runningTotal endOfLastRange ((rangeStart,rangeEnd):list) =
    count (runningTotal + rangeStart-1 - endOfLastRange) rangeEnd list

part2 :: IO Integer
part2 = fmap (count 0 (-1) . getList) $ readFile "input/20.txt"
