{-
--- Day 7: Handy Haversacks ---

You land at the regional airport in time for your next flight. In fact, it
looks like you'll even have time to grab some food: all flights are currently
delayed due to issues in luggage processing.

Due to recent aviation regulations, many rules (your puzzle input) are being
enforced about bags and their contents; bags must be color-coded and must
contain specific quantities of other color-coded bags. Apparently, nobody
responsible for these regulations considered how long they would take to
enforce!

For example, consider the following rules:

| light red bags contain 1 bright white bag, 2 muted yellow bags.
| dark orange bags contain 3 bright white bags, 4 muted yellow bags.
| bright white bags contain 1 shiny gold bag.
| muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
| shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
| dark olive bags contain 3 faded blue bags, 4 dotted black bags.
| vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
| faded blue bags contain no other bags.
| dotted black bags contain no other bags.

These rules specify the required contents for 9 bag types. In this example,
every faded blue bag is empty, every vibrant plum bag contains 11 bags (5 faded
blue and 6 dotted black), and so on.

You have a shiny gold bag. If you wanted to carry it in at least one other bag,
how many different bag colors would be valid for the outermost bag? (In other
words: how many colors can, eventually, contain at least one shiny gold bag?)

In the above rules, the following options would be available to you:

 - A bright white bag, which can hold your shiny gold bag directly.
 - A muted yellow bag, which can hold your shiny gold bag directly, plus some
   other bags.
 - A dark orange bag, which can hold bright white and muted yellow bags, either
   of which could then hold your shiny gold bag.
 - A light red bag, which can hold bright white and muted yellow bags, either
   of which could then hold your shiny gold bag.

So, in this example, the number of bag colors that can eventually contain at
least one shiny gold bag is 4.

How many bag colors can eventually contain at least one shiny gold bag? (The
list of rules is quite long; make sure you get all of it.)
-}

import Data.Map(Map,fromList,keys,keysSet,toList,(!))
import Data.Set(member,size)

parse :: String -> Map (String,String) (Map (String,String) Int)
parse = fromList . map (parseRule . words) . lines
  where
    parseRule (c1:c2:"bags":"contain":contents) =
        ((c1,c2),fromList $ parseContents contents)
    parseContents (n:c3:c4:_:rest) = ((c3,c4),read n):parseContents rest
    parseContents _ = []

containedBy :: (String,String) -> Map (String,String) (Map (String,String) Int) -> Map (String,String) Bool
containedBy bag rules = canContain
  where
    canContain = fromList (map makeCanContain $ keys rules)
    makeCanContain container
      | size contents == 0 = (container,False)
      | member bag contents = (container,True)
      | otherwise = (container,any (canContain!) contents)
      where
        contents = keysSet (rules!container)

testData :: String
testData = "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.\n"

test :: ()
test
  | (length . filter snd . toList . containedBy ("shiny","gold") . parse) testData /= 4 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . filter snd . toList . containedBy ("shiny","gold") . parse) $ readFile "input/07.txt"

count :: (String,String) -> Map (String,String) (Map (String,String) Int) -> Int
count bag rules = sum $ map subcounts $ toList $ rules!bag
  where subcounts (containedBag,n) = n*(1 + count containedBag rules)

testData2 :: String
testData2 = "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags.\n"

test2 :: ()
test2
  | (count ("shiny","gold") . parse) testData2 /= 126 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (count ("shiny","gold") . parse) $ readFile "input/07.txt"
