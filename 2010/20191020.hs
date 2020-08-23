import Control.Monad(foldM_,when)
import Data.Map(Map,alter,empty,member,(!))
import Data.List(sort)

search :: Map String [String] -> String -> IO (Map String [String])
search words word = do
    let key = sort word
    let k1 = map pred key
    let k2 = map succ key
    when (member k1 words) (print (word,words!k1))
    when (member k2 words) (print (word,words!k2))
    return (alter (Just . maybe [word] (word:)) key words)

main :: IO ()
main = readFile "/usr/share/dict/words" >>= foldM_ search empty . filter ((== 7) . length) . ("fumbled":) . lines

-- tackled fumbled
