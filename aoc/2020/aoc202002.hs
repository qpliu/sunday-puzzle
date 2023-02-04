import Data.Bits(xor)
import Data.Char(isDigit)

parse :: String -> [(Int,Int,Char,String)]
parse = map (p . words . map (\ c -> if c == '-' then ' ' else c)) . lines
  where
    p (low:high:char:pwd:_) = (read low,read high,head char,pwd)
    p line = error (show line)

valid :: (Int,Int,Char,String) -> Bool
valid (low,high,char,pwd) = count >= low && count <= high
  where count = length (filter (== char) pwd)

testData :: String
testData = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc\n"

test :: ()
test
  | (length . filter valid . parse) testData /= 2 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . filter valid . parse) $ readFile "input/02.txt"

valid2 :: (Int,Int,Char,String) -> Bool
valid2 (i,j,char,pwd) = (char == pwd !! (i-1)) `xor` (char == pwd !! (j-1))

test2 :: ()
test2
  | (length . filter valid2 . parse) testData /= 1 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (length . filter valid2 . parse) $ readFile "input/02.txt"
