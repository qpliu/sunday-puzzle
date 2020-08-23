vowels :: Int -> String -> Bool
vowels n s = length (filter (flip elem s) "aeiou") >= n

consonants :: String -> Bool
consonants s = case filter (flip notElem "aeiou") s of
  [a,b,c] -> ((a == b) /= (a == c)) /= (b == c)
  otherwise -> False

collect :: ([String],[String]) -> String -> ([String],[String])
collect set@(set2,set6) word
  | length word == 2 = (word:set2,set6)
  | length word == 6 && vowels 4 word = (set2,word:set6)
  | length word == 6 && consonants word && vowels 3 word = (set2,word:set6)
  | length word == 5 && vowels 4 word = (set2,(word++"d"):(word++"s"):set6)
  | length word == 4 && vowels 4 ('e':word) = (set2,(word ++ "ed"):(word ++ "es"):set6)
  | length word == 4 && drop 3 word == "y" && vowels 4 ("ie" ++ take 3 word) = (set2,(take 3 word ++ "ies"):(take 3 word ++ "ied"):set6)
  | otherwise = set

find :: ([String],[String]) -> [String]
find (_,set6) = [w2 ++ " " ++ w6 | w2 <- set2, w6 <- set6,
                                      vowels 5 (w2++w6), consonants (w2++w6)]
  where
    set2 = ["ab","ad","al","am","an","as","at","au","ay","be",
            "by","de","di","do","et","ex","go","hi","if","in",
            "is","it","la","le","me","my","no","of","oh","on",
            "or","se","si","so","to","te","up","us","we"]

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . find . foldl collect ([],["revoir"]) . lines

-- au revoir
