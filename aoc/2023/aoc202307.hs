import Data.List(sort)

data HandType = High | Pair | TwoPair | Three | Full | Four | Five
  deriving (Eq,Ord,Show)

parse :: String -> [((HandType,[Int]),Int)]
parse = map (parseItem . words) . lines
  where
    parseItem [hand,bid] = ((handType $ sort labels,labels),read bid)
      where labels = map toLabel hand
    toLabel '2' = 2
    toLabel '3' = 3
    toLabel '4' = 4
    toLabel '5' = 5
    toLabel '6' = 6
    toLabel '7' = 7
    toLabel '8' = 8
    toLabel '9' = 9
    toLabel 'T' = 10
    toLabel 'J' = 11
    toLabel 'Q' = 12
    toLabel 'K' = 13
    toLabel 'A' = 14
    handType [a,b,c,d,e]
      | a == e = Five
      | a == d = Four
      | b == e = Four
      | a == c && d == e = Full
      | a == b && c == e = Full
      | a == c = Three
      | b == d = Three
      | c == e = Three
      | a == b && c == d = TwoPair
      | a == b && d == e = TwoPair
      | b == c && d == e = TwoPair
      | a == b = Pair
      | b == c = Pair
      | c == d = Pair
      | d == e = Pair
      | otherwise = High

result :: String -> Int
result = sum . zipWith (*) [1..] . map snd . sort . parse

testData :: String
testData = unlines [
    "32T3K 765","T55J5 684","KK677 28","KTJJT 220","QQQJA 483"
    ]

test :: ()
test
  | result testData /= 6440 = error "a"
  | otherwise = ()
  where

part1 :: IO Int
part1 = fmap result $ readFile "input/07.txt"

parse2 :: String -> [((HandType,[Int]),Int)]
parse2 = map (parseItem . words) . lines
  where
    parseItem [hand,bid] = ((handType $ sort labels,labels),read bid)
      where labels = map toLabel hand
    toLabel '2' = 2
    toLabel '3' = 3
    toLabel '4' = 4
    toLabel '5' = 5
    toLabel '6' = 6
    toLabel '7' = 7
    toLabel '8' = 8
    toLabel '9' = 9
    toLabel 'T' = 10
    toLabel 'J' = 1
    toLabel 'Q' = 12
    toLabel 'K' = 13
    toLabel 'A' = 14
    handType [a,b,c,d,e]
      | a == e = Five
      | a == d && a == 1 = Five
      | a == d = Four
      | b == e && a == 1 = Five
      | b == e = Four
      | a == c && d == e && a == 1 = Five
      | a == c && d == e = Full
      | a == b && c == e && a == 1 = Five
      | a == b && c == e = Full
      | a == c && a == 1 = Four
      | a == c = Three
      | b == d && a == 1 = Four
      | b == d = Three
      | c == e && a == 1 = Four
      | c == e = Three
      | a == b && c == d && a == 1 = Four
      | a == b && c == d = TwoPair
      | a == b && d == e && a == 1 = Four
      | a == b && d == e = TwoPair
      | b == c && d == e && a == 1 = Full
      | b == c && d == e = TwoPair
      | a == b && a == 1 = Three
      | a == b = Pair
      | b == c && a == 1 = Three
      | b == c = Pair
      | c == d && a == 1 = Three
      | c == d = Pair
      | d == e && a == 1 = Three
      | d == e = Pair
      | a == 1 = Pair
      | otherwise = High

result2 :: String -> Int
result2 = sum . zipWith (*) [1..] . map snd . sort . parse2

test2 :: ()
test2
  | result2 testData /= 5905 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/07.txt"
