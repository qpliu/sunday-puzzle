import Data.Char(toLower)
import Data.List(nub)
import Data.Map(Map,adjust,fromList,(!))
import Data.Set(Set,elems,empty,insert,member)

collect :: Map Char (Set String) -> String -> Map Char (Set String)
collect map word
  | length word /= 4 || length (nub word) /= 4
                     || any (`elem` "nagsewt") (tail word) = map
  | otherwise = adjust (insert (tail word)) (head word) map

results :: Map Char (Set String) -> [(String,String,String)]
results map = [(e,w,t) | e <- elems (map ! 'e'),
                         w <- elems (map ! 'w'),
                         t <- elems (map ! 't'),
                         length (nub (e ++ w ++ t)) == 9,
                         [e !! 0, w !! 0, t !! 0] `member` (map ! 'a'),
                         [e !! 1, w !! 1, t !! 1] `member` (map ! 'g'),
                         [e !! 2, w !! 2, t !! 2] `member` (map ! 's')]

puz :: String -> [(String,String,String)]
puz words = results $ foldl collect (fromList (zip "agsewt" (repeat empty)))
                    $ lines $ map toLower words

main :: IO ()
main = fmap puz (readFile "/usr/share/dict/words") >>= print
