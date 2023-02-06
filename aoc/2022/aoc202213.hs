import Data.List(sort)

data Packet = I Int | L [Packet] deriving (Eq,Show)

parsePacket :: String -> Packet
parsePacket str =
    let (packet,rest) = p str in if null rest then packet else error str
  where
    p ('[':rest)
      | take 1 rest2 == "]" = (L packets,drop 1 rest2)
      | otherwise = error (show rest)
      where (packets,rest2) = pList [] rest
    p string = (I (read i),rest)
      where (i,rest) = span (not . (`elem` ",]")) string
    pList items string
      | take 1 string == "]" = (reverse items,string)
      | otherwise = pList (item:items) (dropWhile (== ',') rest)
      where (item,rest) = p string
      
instance Ord Packet where
    (<=) (I l) (I r) = l <= r
    (<=) l@(I _) r = L [l] <= r
    (<=) l r@(I _) = l <= L [r]
    (<=) (L []) (L _) = True
    (<=) _ (L []) = False
    (<=) (L (l:ls)) (L (r:rs))
      | not (l <= r) =  False
      | r <= l = L ls <= L rs
      | otherwise = True

parse :: String -> [(Packet,Packet)]
parse = p . lines
  where
    p (p1:p2:rest) = (parsePacket p1,parsePacket p2) : p (drop 1 rest)
    p _ = []

testData :: String
testData = unlines [
    "[1,1,3,1,1]",
    "[1,1,5,1,1]",
    "",
    "[[1],[2,3,4]]",
    "[[1],4]",
    "",
    "[9]",
    "[[8,7,6]]",
    "",
    "[[4,4],4,4]",
    "[[4,4],4,4,4]",
    "",
    "[7,7,7,7]",
    "[7,7,7]",
    "",
    "[]",
    "[3]",
    "",
    "[[[]]]",
    "[[]]",
    "",
    "[1,[2,[3,[4,[5,6,7]]]],8,9]",
    "[1,[2,[3,[4,[5,6,0]]]],8,9]"
    ]

test :: ()
test
  | (map fst . filter snd . zip [1..] . map (uncurry (<=)) . parse) testData /= [1,2,4,6] = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . map fst . filter snd . zip [1..] . map (uncurry (<=)) . parse) $ readFile "input/13.txt"

parse2 :: String -> [Packet]
parse2 = map parsePacket . filter (/= "") . lines

run2 :: [Packet] -> Int
run2 packets = i2*i6
  where
    list = zip (sort (L [I 2]:L [I 6]:packets)) [1..]
    Just i2 = lookup (L [I 2]) list
    Just i6 = lookup (L [I 6]) list

test2 :: ()
test2
  | (run2 . parse2) testData /= 140 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (run2 . parse2) $ readFile "input/13.txt"
