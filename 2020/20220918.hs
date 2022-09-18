import Data.Set(Set,empty,insert,member)

collect :: ([String],Set String) -> String -> ([String],Set String)
collect (results,set) word =
  case reverse word of
    ('m':w) -> (results,insert w set)
    ('n':'r':w) -> (if w `member` set then (reverse w++"m/"++reverse w++"rn"):results else results,set)
    _ -> (results,set)

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ putStrLn . fst . foldl collect ([],empty) . lines

-- darn dam damn
