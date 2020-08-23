import Data.List(sort)
import Data.Map(Map,adjust,fromList,(!))
import Data.Maybe(catMaybes)

containedIn :: Ord a => [a] -> [a] -> Bool
containedIn [] _ = True
containedIn _ [] = False
containedIn (l:ls) n@(m:ms)
  | l == m = containedIn ls ms
  | l > m = containedIn ls n
  | otherwise = False

possibleWords :: String -> [String] -> Map Int [String]
possibleWords sortedName words = foldl addWord empty words
  where
    empty = fromList [(n,[]) | n <- [1 .. length words]]
    addWord map word =
        if (sort word) `containedIn` sortedName
          then adjust (word:) (length word) map
          else map

filteredPairs :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
filteredPairs f as bs = [(a,b) | a <- as, b <- bs, f a b]

matches :: String -> [String] -> [(String,String)]
matches sortedName words =
  let lists = possibleWords sortedName words
  in  concat [filteredPairs (\ a b -> sort (a++b) == sortedName)
                (lists!n) (lists!(length sortedName - n))
              | n <- [1..length sortedName `div` 2]]

printMatches :: [String] -> String -> IO ()
printMatches words name = do
    print name
    mapM_ print (matches (sort name) words)

main :: IO ()
main = do
    words <- fmap lines (readFile "/usr/share/dict/words")
    mapM_ (printMatches words)
        ["amy", "cate", "judi", "meryl", "sandra", "bruce", "chiwetel",
	 "christian", "leonardo", "matthew"]

-- sandra: ran sad
-- chiwetel: eel witch, chew tile
-- christian: air snitch, stir china, stir chain, rich stain, rich saint
-- leonardo: on ordeal, no ordeal, ale donor, loan doer, do loaner, do reloan
