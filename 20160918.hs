import Data.Set(Set,empty,insert,member,toList)

collect :: (Set String,Set String,Set String,Set String,Set String,Set String,Set String) -> String -> (Set String,Set String,Set String,Set String,Set String,Set String,Set String)
collect (w1,w2,w3,w4,w5,w6,w7) word
  | length word == 1 = (insert word w1,w2,w3,w4,w5,w6,w7)
  | length word == 2 = (w1,insert word w2,w3,w4,w5,w6,w7)
  | length word == 3 = (w1,w2,insert word w3,w4,w5,w6,w7)
  | length word == 4 = (w1,w2,w3,insert word w4,w5,w6,w7)
  | length word == 5 = (w1,w2,w3,w4,insert word w5,w6,w7)
  | length word == 6 = (w1,w2,w3,w4,w5,insert word w6,w7)
  | length word == 7 = (w1,w2,w3,w4,w5,w6,insert word w7)
  | otherwise = (w1,w2,w3,w4,w5,w6,w7)

search :: Set String -> [String] -> [String] -> [String]
search w7 wa wb =
    [a ++ b ++ " " ++ b ++ " and " ++ a | a <- wa, b <- wb, member (a ++ b) w7] ++
    [b ++ a ++ " " ++ a ++ " and " ++ b | a <- wa, b <- wb, member (b ++ a) w7]

main :: IO ()
main = do
    (w1,w2,w3,w4,w5,w6,w7) <- fmap (foldl collect (empty,empty,empty,empty,empty,empty,empty) . lines) (readFile "/usr/share/dict/words")
    mapM_ print (search w7 (toList w3) (toList w4))

-- here and now -> nowhere

