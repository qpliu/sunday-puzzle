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
