{-
--- Day 9: Explosives in Cyberspace ---

Wandering around a secure area, you come across a datalink port to a new part
of the network. After briefly scanning it for interesting files, you find one
file in particular that catches your attention. It's compressed with an
experimental format, but fortunately, the documentation for the format is
nearby.

The format compresses a sequence of characters. Whitespace is ignored. To
indicate that some sequence should be repeated, a marker is added to the file,
like (10x2). To decompress this marker, take the subsequent 10 characters and
repeat them 2 times. Then, continue reading the file after the repeated data.
The marker itself is not included in the decompressed output.

If parentheses or other characters appear within the data referenced by a
marker, that's okay - treat it like normal data, not a marker, and then resume
looking for markers after the decompressed section.

For example:

 - ADVENT contains no markers and decompresses to itself with no changes,
   resulting in a decompressed length of 6.
 - A(1x5)BC repeats only the B a total of 5 times, becoming ABBBBBC for a
   decompressed length of 7.
 - (3x3)XYZ becomes XYZXYZXYZ for a decompressed length of 9.
 - A(2x2)BCD(2x2)EFG doubles the BC and EF, becoming ABCBCDEFEFG for a
   decompressed length of 11.
 - (6x1)(1x3)A simply becomes (1x3)A - the (1x3) looks like a marker, but
   because it's within a data section of another marker, it is not treated any
   differently from the A that comes after it. It has a decompressed length of
   6.
 - X(8x2)(3x3)ABCY becomes X(3x3)ABC(3x3)ABCY (for a decompressed length of
   18), because the decompressed data from the (8x2) marker (the (3x3)ABC) is
   skipped and not processed further.

What is the decompressed length of the file (your puzzle input)? Don't count
whitespace.
-}

import Data.Char(isDigit,isSpace)

decompressedLength :: String -> Int
decompressedLength s = count (filter (not . isSpace) s)
  where
    count "" = 0
    count ('(':s1) = read len*read repeats + count (drop (1 + read len) s3)
      where
        (len,s2) = span isDigit s1
        (repeats,s3) = span isDigit (dropWhile (not . isDigit) s2)
    count (_:s1) = 1 + count s1

test :: ()
test
  | decompressedLength "ADVENT" /= 6 = error "a"
  | decompressedLength "A(1x5)BC" /= 7 = error "b"
  | decompressedLength "(3x3)XYZ" /= 9 = error "c"
  | decompressedLength "A(2x2)BCD(2x2)EFG" /= 11 = error "d"
  | decompressedLength "(6x1)(1x3)A" /= 6 = error "d"
  | decompressedLength "X(8x2)(3x3)ABCY" /= 18 = error "d"
  | otherwise = ()

part1 :: IO Int
part1 = fmap decompressedLength $ readFile "input/09.txt"

decom2Len :: String -> Int
decom2Len s = count (filter (not . isSpace) s)
  where
    count "" = 0
    count ('(':s1) = repeats*decom2Len (take len (tail s3)) + count (drop (1 + len) s3)
      where
        (lenStr,s2) = span isDigit s1
        (repeatsStr,s3) = span isDigit (dropWhile (not . isDigit) s2)
        len = read lenStr
        repeats = read repeatsStr
    count (_:s1) = 1 + count s1

test2 :: ()
test2
  | decom2Len "(3x3)XYZ" /= 9 = error "a"
  | decom2Len "X(8x2)(3x3)ABCY" /= 20 = error "b"
  | decom2Len "(27x12)(20x12)(13x14)(7x10)(1x12)A" /= 241920 = error "c"
  | decom2Len "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" /= 445 = error "d"
  | otherwise = ()

part2 :: IO Int
part2 = fmap decom2Len $ readFile "input/09.txt"
