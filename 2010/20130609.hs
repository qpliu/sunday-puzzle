import Data.Char(chr,ord,toLower)
import Data.List(sort)
import Data.Map(Map,alter,empty,toList)
import qualified Data.Map

key :: String -> String
key = sort . map toLower

collect :: Map String [String] -> String -> Map String [String]
collect dict word
  | length word /= 5 = dict
  | head word `elem` "aeiouAEIOU" = alter (Just . maybe [word] (word:)) (key word) dict
  | otherwise = dict

searchKeys :: String -> [String]
searchKeys = skeys ""
  where
    skeys cs1 (c:cs) = sort ((chr . (+2) . ord) c : cs1 ++ cs) : skeys (c:cs1) cs
    skeys _ [] = []

search :: Map String [String] -> IO ()
search dict = mapM_ search1 (toList dict)
  where
    search1 (key,words) = mapM_ (maybe (return ()) (print . (,) words)) (map (flip Data.Map.lookup dict) (searchKeys key))

main :: IO ()
main = readFile "/usr/share/dict/words" >>= search . foldl collect empty . lines

-- After Earth
