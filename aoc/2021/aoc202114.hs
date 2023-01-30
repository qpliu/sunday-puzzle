{-
--- Day 14: Extended Polymerization ---

The incredible pressures at this depth are starting to put a strain on your
submarine. The submarine has polymerization equipment that would produce
suitable materials to reinforce the submarine, and the nearby
volcanically-active caves should even have the necessary input elements in
sufficient quantities.

The submarine manual contains instructions for finding the optimal polymer
formula; specifically, it offers a polymer template and a list of pair
insertion rules (your puzzle input). You just need to work out what polymer
would result after repeating the pair insertion process a few times.

For example:

| NNCB
| 
| CH -> B
| HH -> N
| CB -> H
| NH -> C
| HB -> C
| HC -> B
| HN -> C
| NN -> C
| BH -> H
| NC -> B
| NB -> B
| BN -> B
| BB -> N
| BC -> B
| CC -> N
| CN -> C

The first line is the polymer template - this is the starting point of the
process.

The following section defines the pair insertion rules. A rule like AB -> C
means that when elements A and B are immediately adjacent, element C should be
inserted between them. These insertions all happen simultaneously.

So, starting with the polymer template NNCB, the first step simultaneously
considers all three pairs:

 - The first pair (NN) matches the rule NN -> C, so element C is inserted
   between the first N and the second N.
 - The second pair (NC) matches the rule NC -> B, so element B is inserted
   between the N and the C.
 - The third pair (CB) matches the rule CB -> H, so element H is inserted
   between the C and the B.

Note that these pairs overlap: the second element of one pair is the first
element of the next pair. Also, because all pairs are considered
simultaneously, inserted elements are not considered to be part of a pair until
the next step.

After the first step of this process, the polymer becomes NCNBCHB.

Here are the results of a few steps using the above rules:

| Template:     NNCB
| After step 1: NCNBCHB
| After step 2: NBCCNBBBCBHCB
| After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
| After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB

This polymer grows quickly. After step 5, it has length 97; After step 10, it
has length 3073. After step 10, B occurs 1749 times, C occurs 298 times, H
occurs 161 times, and N occurs 865 times; taking the quantity of the most
common element (B, 1749) and subtracting the quantity of the least common
element (H, 161) produces 1749 - 161 = 1588.

Apply 10 steps of pair insertion to the polymer template and find the most and
least common elements in the result. What do you get if you take the quantity
of the most common element and subtract the quantity of the least common
element?
-}

import Data.Map(Map,alter,empty,filterWithKey,fromList,keys,mapKeys,member,unionsWith,(!))

parse :: String -> (String,Map (Char,Char) Char)
parse str = (template,fromList (makeRules rules))
  where
    (template:rules) = words str
    makeRules ([a,b]:"->":[c]:rest) = ((a,b),c) : makeRules rest
    makeRules _ = []

step :: Map (Char,Char) Char -> String -> String
step rules [] = []
step rules [a] = [a]
step rules (a:rest@(b:_))
  | member (a,b) rules = a:rules!(a,b):step rules rest
  | otherwise = a:step rules rest

tabulate :: Ord a => [a] -> Map a Int
tabulate = foldr (alter (Just . maybe 1 (+1))) empty

testData :: String
testData = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C\n"

test :: ()
test
  | (head . drop 1 . iterate (step rules)) template /= "NCNBCHB" = error "a"
  | (head . drop 2 . iterate (step rules)) template /= "NBCCNBBBCBHCB" = error "b"
  | (head . drop 3 . iterate (step rules)) template /= "NBBBCNCCNBBNBNBBCHBHHBCHB" = error "c"
  | (head . drop 4 . iterate (step rules)) template /= "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB" = error "d"
  | (length . head . drop 5 . iterate (step rules)) template /= 97 = error "e"
  | (length . head . drop 10 . iterate (step rules)) template /= 3073 = error "f"
  | table!'B' /= 1749 = error "g"
  | table!'C' /= 298 = error "h"
  | table!'H' /= 161 = error "i"
  | table!'N' /= 865 = error "j"
  | maximum table - minimum table /= 1588 = error "k"
  | otherwise = ()
  where
    (template,rules) = parse testData
    table = (tabulate . head . drop 10 . iterate (step rules)) template

part1 :: IO Int
part1 = do
    (template,rules) <- fmap parse $ readFile "input/14.txt"
    let table = (tabulate . head . drop 10 . iterate (step rules)) template
    return $ maximum table - minimum table

counts :: Int -> Map (Char,Char) Char -> Map (Char,Char) (Map Char Int)
counts maxLevel rules = mapKeys snd $ filterWithKey (\ (level,_) _ -> level == 0) table
  where
    table = fromList [((level,pair),getCounts level pair) | level <- [0..maxLevel], pair <- keys rules]
    getCounts level (left,right)
      | level >= maxLevel = empty
      | otherwise = unionsWith (+) [fromList [(middle,1)],table!(level+1,(left,middle)),table!(level+1,(middle,right))]
      where middle = rules!(left,right)

makeTable :: Int -> (String,Map (Char,Char) Char) -> Map Char Int
makeTable maxLevel (template,rules) = unionsWith (+) (tabulate template:map (c!) (zip template (drop 1 template)))
  where
    c = counts maxLevel rules

test2 :: ()
test2
  | table10!'B' /= 1749 = error "a"
  | table10!'C' /= 298 = error "b"
  | table10!'H' /= 161 = error "c"
  | table10!'N' /= 865 = error "d"
  | table40!'B' /= 2192039569602 = error "e"
  | table40!'H' /= 3849876073 = error "f"
  | otherwise = ()
  where
    table10 = (makeTable 10 . parse) testData
    table40 = (makeTable 40 . parse) testData

part2 :: IO Int
part2 = do
    table <- fmap (makeTable 40 . parse) $ readFile "input/14.txt"
    return $ maximum table - minimum table
