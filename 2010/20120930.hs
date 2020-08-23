import Data.List(sort)
import Data.Map(Map,alter,empty)
import qualified Data.Map as M

collectr :: Map String [String] -> String -> Map String [String]
collectr r word@(first:'r':rest) =
    alter (Just . maybe [word] (word:)) (first:sort ('m':rest)) r
collectr r _ = r

collectm :: Map String [String] -> String -> Map String [String]
collectm m word@(first:rest)
  | 'm' `elem` rest = alter (Just . maybe [word] (word:)) (first:sort rest) m
  | otherwise = m

searchr :: Map String [String] -> String -> [(String,String)]
searchr m word@(first:'r':rest) =
    maybe [] (map ((,) word)) (M.lookup (first:sort ('m':rest)) m)
searchr _ _ = []

searchm :: Map String [String] -> String -> [(String,String)]
searchm r word@(first:rest) = maybe [] (map ((,) word)) (M.lookup (first:sort rest) r)

candidates :: Map String [String] -> Map String [String] -> [String] -> [(String,String)]
candidates r m (word:words) =
    searchr m word ++ searchm r word ++ candidates (collectr r word) (collectm m word) words
candidates _ _ [] = []

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . candidates empty empty . lines
