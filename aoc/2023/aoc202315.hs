import Data.Char(ord)
import Data.Map(Map,adjust,fromList,toList)

hashes :: String -> [Int]
hashes = h 0
  where
    h n [] = [n]
    h n (c:cs)
      | c == '\n' = h n cs
      | c == ',' = n : h 0 cs
      | otherwise = h (((n + ord c)*17) `mod` 256) cs

result :: String -> Int
result = sum . hashes

testData :: String
testData = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

test :: ()
test
  | hashes "HASH" /= [52] = error "a"
  | hashes testData /= [30,253,97,47,14,180,9,197,48,214,231] = error "a"
  | result testData /= 1320 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/15.txt"

hash :: String -> Int
hash = h 0
  where
    h n [] = n
    h n (c:cs)
      | c == '\n' = h n cs
      | otherwise = h (((n + ord c)*17) `mod` 256) cs

parse2 :: String -> [(String,Maybe Int)]
parse2 = p . filter (/= '\n')
  where
    p "" = []
    p "\n" = []
    p input
      | last op == '-' = (init op,Nothing) : p (dropWhile (== ',') rest)
      | otherwise = (label,Just (read (dropWhile (== '=') focallen))) : p (dropWhile (== ',') rest)
      where
        (op,rest) = span (/= ',') input
        (label,focallen) = span (/= '=') op

initSeq :: Map Int [(String,Int)] -> (String,Maybe Int) -> Map Int [(String,Int)]
initSeq boxes (label,Nothing) = adjust (filter ((/= label) . fst)) (hash label) boxes
initSeq boxes (label,Just focallen) = adjust replace (hash label) boxes
  where
    replace [] = [(label,focallen)]
    replace ((lab,flen):rest)
      | lab == label = (label,focallen) : rest
      | otherwise = (lab,flen) : replace rest

focusingpower :: (Int,[(String,Int)]) -> Int
focusingpower (box,lenses) = (box+1)*sum (zipWith (*) [1..] $ map snd lenses)

result2 :: String -> Int
result2 = sum . map focusingpower . toList . foldl initSeq (fromList $ zip [0..255] $ repeat []) . parse2

test2 :: ()
test2
  | result2 testData /= 145 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/15.txt"
