import Control.Monad(foldM_)
import Data.Set(Set,empty,member,insert)

collect :: Set String -> String -> IO (Set String)
collect words word
  | member (tail word ++ [head word]) words = do
    print (tail word ++ [head word],word)
    return (insert word words)
  | otherwise = return (insert word words)

main :: IO ()
main = readFile "/usr/share/dict/words" >>= foldM_ collect empty . lines

-- craps table scrap
