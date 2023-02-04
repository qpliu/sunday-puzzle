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
