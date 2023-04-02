import Data.Set(Set,empty,insert,member)

search :: Set String -> [String] -> [(String,String)]
search set (word@(a:b:c:d:e):words)
  | length e /= 4 = search set words
  | member (a:d:c:b:e) set = (word,(a:d:c:b:e)) : search set words
  | otherwise = search (insert word set) words
search set (_:words) = search set words
search set [] = []

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . search empty . words

-- satirist/sitarist
