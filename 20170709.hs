import Data.Set(Set,empty,insert,member)

collect :: Set String -> [String] -> [(Char,String,String)]
collect _ [] = []
collect set (word:words)
  | length word == 7 = collect7 word set ++ collect (insert word set) words
  | length word == 6 = collect6 word set ++ collect (insert word set) words
  | otherwise = collect set words

collect7 :: String -> Set String -> [(Char,String,String)]
collect7 word set
  | member (tail word) set = [(head word,tail word,word)]
  | otherwise = []

collect6 :: String -> Set String -> [(Char,String,String)]
collect6 word set = (map cons . filter has7) ['a' .. 'z']
  where
    cons l = (l,word,l:word)
    has7 l = member (l:word) set

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . collect empty . lines

// f actual factual
