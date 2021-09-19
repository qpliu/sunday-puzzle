import Data.List(nub,sortBy)
import Data.Map(Map,alter,empty)

collect :: Map String [String] -> String -> Map String [String]
collect m word@(a:b:c:rest)
  | [c] == take 1 rest = alter add (a:c:drop 1 rest) m
  | otherwise = alter add (a:c:rest) m
  where add list = Just (maybe [word] (word:) list)
collect m _ = m

display :: [String] -> IO ()
display words | length (nub (map length words)) > 1 && length (nub (map (take 2) words)) > 1 = print (sortBy len words) | otherwise = return ()
  where len a b = compare (length a) (length b)

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ display . foldl collect empty . lines

-- radish reddish
