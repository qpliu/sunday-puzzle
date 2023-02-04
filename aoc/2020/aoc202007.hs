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
