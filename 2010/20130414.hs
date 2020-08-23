import Control.Monad(foldM_)
import Data.Maybe(catMaybes)
import Data.Set(Set,empty,insert,member)

rotate :: Char -> Maybe Char
rotate char = lookup char [('e','w'),('w','e')]-- [('c','u'),('e','m')('h','i'),('i','h'),('m','e'),('n','z'),('o','o'),('u','c'),('x','x'),('z','n')]

transform :: String -> Maybe String
transform word = fmap ((tail word ++) . (:[])) (rotate (head word))

transforms :: String -> [String]
transforms word =
    catMaybes [transform word,fmap reverse (transform (reverse word))]

collect :: Set String -> String -> IO (Set String)
collect words word
  | null matches = return words
  | otherwise = mapM_ printPair matches >> return (insert word words)
  where
    matches = transforms word
    printPair match
      | member match words = print (word,match)
      | otherwise = return ()

main :: IO ()
main = readFile "/usr/share/dict/words" >>= foldM_ collect empty . lines

-- WRY RYE
