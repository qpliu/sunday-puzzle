import Control.Monad(foldM_)
import Data.Char(isAlpha,toLower)
import Data.List(sort)
import Data.Map(Map,empty,insert,lookup,member)
import Prelude hiding (lookup)

search :: (Map String String,Map String String) -> String -> IO (Map String String,Map String String)
search (bands,acbands) name = do
    maybe (return ()) (print . ((,) name)) (lookup key acbands)
    maybe (return ()) (print . ((,) name)) (lookup ackey bands)
    return (insert key name bands,insert ackey name acbands)
  where
    key = sort (map toLower (filter isAlpha name))
    ackey = sort ("ac" ++ (map toLower (filter isAlpha (drop 3 name))) ++ (map toLower (drop 1 (take 2 name))))

main :: IO ()
main = foldM_ search (empty,empty) [
    "The Rolling Stones", "The Beach Boys", "Guns N' Roses",
    "Creedence Clearwater Revival", "Steve Miller Band", "The Doobie Brothers",
    "The White Stripes", "The Black Keys", "Dave Matthews Band",
    "Alice in Chains", "Nine Inch Nails", "Stone Temple Pilots",
    "The Smashing Pumpkins", "Goo Goo Dolls", "Yeah Yeah Yeahs"
    ]

-- Nine Inch Nails Alice in Chains
