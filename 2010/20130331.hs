import Control.Monad(foldM_,when)
import Data.List(sort)
import Data.Map(Map,empty,insert)
import qualified Data.Map

key4 :: String -> Maybe String
key4 word | length word == 4 = Just (sort word) | otherwise = Nothing

key7 :: String -> Maybe String
key7 word | length word == 7 && length key == 4 = Just key | otherwise = Nothing
  where
    key = extract "hmo" (sort word)
    extract [] letters = letters
    extract (x:xs) (l:ls)
      | x == l = extract xs ls
      | otherwise = l:extract (x:xs) ls
    extract _ _ = []

collect :: Map String ([String],[String]) -> String -> IO (Map String ([String],[String]))
collect map word = maybe (maybe (return map) add7 (key7 word)) add4 (key4 word)
  where
    add4 key = maybe (return (insert key ([word],[]) map)) (update4 key) (Data.Map.lookup key map)
    update4 key (words4,words7) = when (not (null words7)) (print (word:words4,words7)) >> return (insert key (word:words4,words7) map)
    add7 key = maybe (return (insert key ([],[word]) map)) (update7 key) (Data.Map.lookup key map)
    update7 key (words4,words7) = when (not (null words4)) (print (words4,word:words7)) >> return (insert key (words4,word:words7) map)

main :: IO ()
main = readFile "/usr/share/dict/words" >>= foldM_ collect empty . lines

-- soap shampoo
