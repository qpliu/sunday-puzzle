import Data.List(sort)
import Data.Map(Map,alter,assocs,empty,member,(!))

collect :: (Map String [String],Map String [String],Map String [String]) -> String -> (Map String [String],Map String [String],Map String [String])
collect (endWithE,startWithE,relevant) word
  | length word == 8 = (endWithE,startWithE,add word word relevant)
  | drop 4 word == "e" = (add (take 4 word) word endWithE,startWithE,relevant)
  | length word == 5 && head word == 'e' = (endWithE,add (drop 1 word) word startWithE,relevant)
  | otherwise = (endWithE,startWithE,relevant)
  where
    add key word dict = alter (Just . maybe [word] (word:)) (sort key) dict

search :: (Map String [String],Map String [String],Map String [String]) -> [([String],[String],[String])]
search (endWithE,startWithE,relevant) = [(endWithEWords,startWithEWords,relevant!sort (endWithEKey++startWithEKey)) | (endWithEKey,endWithEWords) <- assocs endWithE, (startWithEKey,startWithEWords) <- assocs startWithE, member (sort (endWithEKey++startWithEKey)) relevant]

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . search . foldl collect (empty,empty,empty) . lines

-- enter leave relevant
