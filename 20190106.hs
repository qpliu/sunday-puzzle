import Control.Monad(foldM_)
import Data.Char(isAlpha,toLower)
import Data.List(sort)
import Data.Map(Map,alter,empty)
import qualified Data.Map as M

cities :: [(String,String)]
cities = [((sort . map toLower . filter isAlpha) city,city) | city <- [
    "Washington", "Los Angeles", "Pittsburgh", "New Orleans",
    "Cincinnati", "Kansas City", "Sacramento", "Saint Louis",
    "Louisville", "Baton Rouge", "Charleston", "San Antonio"
    ]]

check :: Map String [String] -> String -> [(String,String)] -> IO ()
check words word [] = return ()
check words word ((cityLetters,city):cities) = do
    maybe (return ()) (print . ((,) (city,word)))
          (maybe Nothing (`M.lookup` words) (remainder (sort word) cityLetters))
    check words word cities

remainder :: String -> String -> Maybe String
remainder word city = f word city ""
  where
    f (l:ls) (c:cs) cityStart
      | l == c = f ls cs cityStart
      | otherwise = f (l:ls) cs (c:cityStart)
    f "" cityEnd cityStart = Just (reverse cityStart ++ cityEnd)
    f _ "" _ = Nothing

search :: Map String [String] -> String -> IO (Map String [String])
search words word
  | length word /= 5 = return words
  | otherwise = do
      check words word cities
      return (alter (Just . maybe [word] (word:)) (sort word) words)

main :: IO ()
main = readFile "/usr/share/dict/words" >>= foldM_ search empty . lines

-- Sacramento scent aroma
