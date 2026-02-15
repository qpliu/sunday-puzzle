import Data.Map(Map,alter,empty,member,(!))

search :: Map (String,String) () -> Map (String,String) [String] -> [String] -> [(String,[String])]
search _ _ [] = []
search nines tens (word:words)
  | length word == 9 = try9 ians ++ search newNines tens words
  | length word == 10 = try10 esaus ++ search nines newTens words
  | otherwise = search nines tens words
  where
    ians = [(take i word,drop 3 w) | i <- [0..6],
                                     w <- [drop i word],
                                     take 3 w == "ian"]
    esaus = [(take i word,drop 4 w,take 4 w) | i <- [0..6], w <- [drop i word]]
    newNines = foldr (alter (Just . const ())) nines ians
    newTens = foldr collect tens esaus
      where collect (pre,post,esau) =
                alter (Just . maybe [esau] (esau:)) (pre,post)
    try9 [] = []
    try9 (ian:ians)
      | member ian tens = (word,tens!ian) : try9 ians
      | otherwise = try9 ians
    try10 [] = []
    try10 ((pre,post,esau):esaus)
      | member (pre,post) nines = (word,[esau]) : try10 esaus
      | otherwise = try10 esaus

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . search empty empty . lines

-- appliance applesauce
