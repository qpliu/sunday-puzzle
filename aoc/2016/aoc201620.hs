{-
--- Day 20: Firewall Rules ---

You'd like to set up a small hidden computer here so you can use it to get back
into the network later. However, the corporate firewall only allows
communication with certain external IP addresses.

You've retrieved the list of blocked IPs from the firewall, but the list seems
to be messy and poorly maintained, and it's not clear which IPs are allowed.
Also, rather than being written in dot-decimal notation, they are written as
plain 32-bit integers, which can have any value from 0 through 4294967295,
inclusive.

For example, suppose only the values 0 through 9 were valid, and that you
retrieved the following blacklist:

5-8
0-2
4-7

The blacklist specifies ranges of IPs (inclusive of both the start and end
value) that are not allowed. Then, the only IPs that this firewall allows are 3
and 9, since those are the only numbers not in any range.

Given the list of blocked IPs you retrieved from the firewall (your puzzle
input), what is the lowest-valued IP that is not blocked?
-}

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
