generate :: Int -> String -> String
generate len s
  | length s >= len = take len s
  | otherwise = generate len (s ++ '0':reverse (map rev s))
  where rev c | c == '0' = '1' | otherwise = '0'

checksum :: String -> String
checksum s | odd (length s) = s | otherwise = checksum (reduce s)
  where
    reduce "" = ""
    reduce (a:b:c) | a == b = '1':reduce c | otherwise = '0':reduce c

test :: ()
test
  | generate 3 "1" /= "100" = error "a"
  | generate 3 "0" /= "001" = error "b"
  | generate 11 "11111" /= "11111000000" = error "c"
  | generate 25 "111100001010" /= "1111000010100101011110000" = error "d"
  | checksum "110010110100" /= "100" = error "e"
  | checksum "10000011110010000111" /= "01100" = error "f"
  | checksum (generate 20 "10000") /= "01100" = error "g"
  | otherwise = ()

part1 :: IO String
part1 = fmap (checksum . generate 272 . head . words) $ readFile "input/16.txt"

part2 :: IO String
part2 = fmap (checksum . generate 35651584 . head . words) $ readFile "input/16.txt"
