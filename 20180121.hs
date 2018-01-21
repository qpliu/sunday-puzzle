import Control.Monad(foldM,foldM_)
import Data.List(sort)
import Data.Map(Map,alter,empty,lookup)
import Prelude hiding (lookup)

search :: (Map String [String],Map String [String]) -> String -> IO (Map String [String],Map String [String])
search (l6,l7) word
  | length word == 6 = do
        let key = sort word
        p key word l7
        return (a key word l6,l7)
  | length word == 7 = do
        let key = (sort (take 3 word ++ drop 4 word))
        p key word l6
        return (l6,a key word l7)
  | otherwise = return (l6,l7)

a :: String -> String -> Map String [String] -> Map String [String]
a key value m = alter (Just . maybe [value] (value:)) key m

p :: String -> String -> Map String [String] -> IO ()
p key value m = maybe (return ()) (print . (,) value) (lookup key m)

main :: IO ()
main = do
    words <- fmap lines (readFile "/usr/share/dict/words")
    foldM_ search (empty,empty) words

-- gondola lagoon
