import Data.Char(chr,isAlpha,isDigit,ord)
import Data.List(group,sort)

checksum :: String -> String
checksum s = take 5 $ concatMap snd $ sort $ map count $ group $ sort $ filter isAlpha $ s
  where
    count ls = (- length ls,take 1 ls)

sectorID :: String -> Int
sectorID s
    | "[" ++ checksum s1 ++ "]" == s2 = read (filter isDigit s1)
    | otherwise = 0
  where
    (s1,s2) = span (/= '[') s

test :: ()
test
  | sectorID "aaaaa-bbb-z-y-x-123[abxyz]" /= 123 = error "a"
  | sectorID "a-b-c-d-e-f-g-h-987[abcde]" /= 987 = error "b"
  | sectorID "not-a-real-room-404[oarel]" /= 404 = error "c"
  | sectorID "totally-real-room-200[decoy]" /= 0 = error "d"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . map sectorID . lines) $ readFile "input/04.txt"

-- Fortunately, none of the real sectorIDs are 0.
decrypt :: String -> String
decrypt s
  | shift == 0 = ""
  | otherwise = map (decipher shift) (takeWhile (not . isDigit) s)
  where shift = sectorID s

decipher :: Int -> Char -> Char
decipher shift c
  | not (isAlpha c) = ' '
  | otherwise = chr (ord 'a' + (ord c - ord 'a' + shift) `mod` 26)

part2 :: IO [String]
part2 = fmap (filter (("northpole object storage " ==) . decrypt) . lines) $ readFile "input/04.txt"
