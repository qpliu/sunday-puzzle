snafu :: Char -> Int
snafu '0' = 0
snafu '1' = 1
snafu '2' = 2
snafu '-' = -1
snafu '=' = -2

fromSNAFUacc :: Int -> String -> Int
fromSNAFUacc acc [] = acc
fromSNAFUacc acc (c:rest) = fromSNAFUacc (5*acc+snafu c) rest
  
fromSNAFU :: String -> Int
fromSNAFU = fromSNAFUacc 0

toSNAFUacc :: String -> Int -> String
toSNAFUacc acc n
  | n == 0 = if null acc then "0" else acc
  | n `mod` 5 == 0 = toSNAFUacc ('0':acc) (n `div` 5)
  | n `mod` 5 == 1 = toSNAFUacc ('1':acc) (n `div` 5)
  | n `mod` 5 == 2 = toSNAFUacc ('2':acc) (n `div` 5)
  | n `mod` 5 == 3 = toSNAFUacc ('=':acc) ((n `div` 5) + 1)
  | n `mod` 5 == 4 = toSNAFUacc ('-':acc) ((n `div` 5) + 1)

toSNAFU :: Int -> String
toSNAFU = toSNAFUacc ""

testData :: String
testData = unlines [
    "1=-0-2",
    "12111",
    "2=0=",
    "21",
    "2=01",
    "111",
    "20012",
    "112",
    "1=-1=",
    "1-12",
    "12",
    "1=",
    "122"
    ]

test :: ()
test
  | (toSNAFU . sum . map fromSNAFU . lines) testData /= "2=-1=0" = error "a"
  | toSNAFU 1 /= "1" = error "b"
  | toSNAFU 2 /= "2" = error "b"
  | toSNAFU 3 /= "1=" = error "b"
  | toSNAFU 4 /= "1-" = error "b"
  | toSNAFU 5 /= "10" = error "b"
  | toSNAFU 6 /= "11" = error "b"
  | toSNAFU 7 /= "12" = error "b"
  | toSNAFU 8 /= "2=" = error "b"
  | toSNAFU 9 /= "2-" = error "b"
  | toSNAFU 10 /= "20" = error "b"
  | toSNAFU 15 /= "1=0" = error "b"
  | toSNAFU 20 /= "1-0" = error "b"
  | toSNAFU 2022 /= "1=11-2" = error "b"
  | toSNAFU 12345 /= "1-0---0" = error "b"
  | toSNAFU 314159265 /= "1121-1110-1=0" = error "b"
  | fromSNAFU "1=-0-2" /= 1747 = error "c"
  | fromSNAFU "12111" /= 906 = error "c"
  | fromSNAFU "2=0=" /= 198 = error "c"
  | fromSNAFU "21" /= 11 = error "c"
  | fromSNAFU "2=01" /= 201 = error "c"
  | fromSNAFU "111" /= 31 = error "c"
  | fromSNAFU "20012" /= 1257 = error "c"
  | fromSNAFU "112" /= 32 = error "c"
  | fromSNAFU "1=-1=" /= 353 = error "c"
  | fromSNAFU "1-12" /= 107 = error "c"
  | fromSNAFU "12" /= 7 = error "c"
  | fromSNAFU "1=" /= 3 = error "c"
  | fromSNAFU "122" /= 37 = error "c"
  | otherwise = ()

part1 :: IO String
part1 = fmap (toSNAFU . sum . map fromSNAFU . lines) $ readFile "input/25.txt"
