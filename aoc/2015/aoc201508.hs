excess :: String -> Int
excess s = 1 + count (drop 1 $ dropWhile (/= '"') s)
  where
    count ('\\':'\\':rest) = 1 + count rest
    count ('\\':'"':rest) = 1 + count rest
    count ('\\':'x':_:_:rest) = 3 + count rest
    count ('"':_) = 1
    count (_:rest) = count rest

test :: ()
test
  | excess "\"\"" /= 2 = error "a"
  | excess "\"abc\"" /= 2 = error "b"
  | excess "\"aaa\\\"aaa\"" /= 3 = error "c"
  | excess "\"\\x27\"" /= 5 = error "d"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . map excess . lines) (readFile "input/08.txt")

part2excess :: String -> Int
part2excess s = 2 + count (drop 1 $ dropWhile (/= '"') s)
  where
    count ('\\':'\\':rest) = 2 + count rest
    count ('\\':'"':rest) = 2 + count rest
    count ('\\':'x':_:_:rest) = 1 + count rest
    count ('"':_) = 2
    count (_:rest) = count rest

part2 :: IO Int
part2 = fmap (sum . map part2excess . lines) (readFile "input/08.txt")
