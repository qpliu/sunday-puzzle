import Control.Monad(foldM_)
import Data.Set(Set,empty,insert,member)

search :: Set String -> String -> IO (Set String)
search words word
  | length word `mod` 2 == 0 = return words
  | word1 `member` words = print (word,word1) >> return (insert word words)
  | word2 `member` words = print (word,word2) >> return (insert word words)
  | otherwise = return (insert word words)
  where
    len2 = length word `div` 2
    word1 = take len2 word ++ drop (len2 + 1) word ++ [head (drop len2 word)]
    word2 = take len2 word ++ drop (len2*2) word ++ (take len2 (drop len2 word))

main :: IO ()
main = readFile "/usr/share/dict/words" >>= foldM_ search empty . lines

-- sneak snake
