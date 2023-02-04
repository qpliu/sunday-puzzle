import Data.Char(chr)
import Data.List(sort)

abcs :: [(Int,Int,Int)]
abcs = [(1,13,3),(1,11,12),(1,15,9),(26,-6,12),(1,15,2),(26,-8,1),(26,-4,1),(1,15,13),(1,10,1),(1,11,6),(26,-11,2),(26,0,11),(26,-8,10),(26,-7,3)]

-- Decompiling the code gives:
-- if input == b + z mod 26
-- then z = (z div a)*26 + input + c
-- else z = (z div a)
monad1 :: Int -> (Int,(Int,Int,Int)) -> Int
monad1 z (i,(a,b,c))
  | i /= b + z `mod` 26 = (z `div` a)*26 + i + c
  | otherwise = z `div` a

monad :: [Int] -> Int
monad input = foldl monad1 0 $ zip input abcs

-- Coming into the 14th digit,
-- Need the 14th digit + 7 = z mod 26, and z < 26.
-- Write z in base 26.
-- Need z = (8..16).

-- Coming into the 13th digit, need to have z come out (8..16),
-- z = (8..16,0..25) -- ie. (8..16)*26 + (0..25)
-- Need the 13th digit + 8 = z mod 26.
-- Need z = (8..16,9..17) -- ie. (8..16)*26 + (9..17)

-- Coming into the 12th digit, need to have z come out (8..16,9..17),
-- z = (8..16,9..17,0..25)
-- Need the 12th digit = z mod 26
-- Need z = (8..16,9..17,0..9)
--
-- Coming into the 11th digit, need to have z come out (8..16,9..17,0..9)
-- z = (8..16,9..17,0..9,0..25)
-- Need the 11th digit + 11 = z mod 26
-- Need z = (8..16,9..17,0..9,12..20)
--
-- Coming into the 10th digit, need to have z come out (8..16,9..17,0..9,12..20)
-- z = (8..16,9..17,0..9,7..15), making 10th digit 9 and 11th digit 4
-- Need z = (8..16,9..17,0..9), note that 0..9 does not interfere with 9(+11).
--
-- Coming into the 9th digit, need to have z come out (8..16,9..17,0..9)
-- z = (8..16,9..17,2..10), making 9th digit 8 and 12th digit 9
-- Need z = (8..16,9..17) -- note that 9..17 does not interfere with 8(+10).
--
-- Coming into the 8th digit, need to have z come out (8..16,9..17)
-- z = (8..16,14..22), making the 8th digit 4 and the 13th digit 9
-- Need z = (8..16) -- note that 8..16 does not interfere with 4(+15).
--
-- Coming into the 7th digit, need to have z come out (8..16)
-- Need z = (8..16,5..14)
--
-- Coming into the 6th digit, need to have z come out (8..16,5..14)
-- Need z = (8..16,5..14,9..17)
--
-- Coming into the 5th digit, need to have z come out (8..16,5..14,9..17)
-- z = (8..16,5..14,3..10), making the 5th digit 9 and the 6th digit 2
-- Need z = (8..16,5..14) -- 5..14 does not interfere with 9(+15)
--
-- Coming into the 4th digit, need to have z come out (8..16,5..14)
-- Need z = (8..16,5..14,7..15)
--
-- Coming into the 3rd digit, need to have z come out (8..16,5..14,7..15)
-- z = (8..16,5..14,10..19), making the 3rd digit 6 and the 4th digit 9
-- Need z = (8..16,5..14) -- 5..14 does not interfere with 6(+15)
--
-- Coming into the 2nd digit, need to have z come out (8..16,5..14)
-- z = (8..16,13..21), making the 2nd digit 2 and the 7th digit 9
-- Need z = (8..16) -- 8..16 DOES interfere with 2(+12)
-- Need z = (8..13+15..16)
--
-- Coming into the 1st digit, need to have z come out (8..13+15..16)
-- z = (4..12), making the 1st digit 9 and the 14th digit 5
--
-- Got some of the digits wrong, but the code gets them all right.

search :: [(Int,Int,Int)] -> [(Int,(Int,Int))] -> [(Int,Int)]
search [] _ = []
search all@((a,b,c):rest) z
  | a == 26 = search rest ((currentDigit,(1-b,9-b)):z)
  | otherwise = (currentDigit,min 9 (stackHigh-c)):(stackDigit,min 9 (c+10-stackLow)) : search rest stack
  where
    currentDigit = length all
    ((stackDigit,(stackLow,stackHigh)):stack) = z

runSearch :: [Int]
runSearch = map snd $ sort $ search (reverse abcs) []

part1 :: String
part1 = map (chr . (+48)) runSearch

search2 :: [(Int,Int,Int)] -> [(Int,(Int,Int))] -> [(Int,Int)]
search2 [] _ = []
search2 all@((a,b,c):rest) z
  | a == 26 = search2 rest ((currentDigit,(1-b,9-b)):z)
  | otherwise = (currentDigit,max 1 (stackLow-c)):(stackDigit,max 1 (c+10-stackHigh)) : search2 rest stack
  where
    currentDigit = length all
    ((stackDigit,(stackLow,stackHigh)):stack) = z

runSearch2 :: [Int]
runSearch2 = map snd $ sort $ search2 (reverse abcs) []

part2 :: String
part2 = map (chr . (+48)) runSearch2
