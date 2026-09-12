import Data.List(sort)
import Data.Set(Set,empty,insert,member)

collect :: String -> (Set String,Set String,[String]) -> (Set String,Set String,[String])
collect w (w3,w4,w7)
  | length w == 3 = (insert (sort w) w3,w4,w7)
  | length w == 4 = (w3,insert w w4,w7)
  | length w == 7 = (w3,w4,w:w7)
  | otherwise = (w3,w4,w7)

test :: Set String -> Set String -> String -> Bool
test w3 w4 w = member (sort (take 3 w)) w3 && member (take 4 (reverse w)) w4

main :: IO ()
main = do
    (w3,w4,w7) <- fmap (foldr collect (empty,empty,[]) . lines) (readFile "/usr/share/dict/words")
    mapM_ putStrLn (filter (test w3 w4) w7)

-- spirits
