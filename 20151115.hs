import Data.Set(Set,elems,fromList,member)

replacements :: String -> [String]
replacements word = rep "" word
  where
    rep pre (a:b:c:rest) | succ a == b && succ b == c = reps (reverse pre) rest
                         | otherwise = rep (a:pre) (b:c:rest)
    rep _ _ = []
    reps pre post = map ((pre++) . (:post)) ['a'..'z']

answers :: Set String -> String -> (String,[String])
answers words word = ((,) word . filter (`member` words) . replacements) word

display :: (String,[String]) -> IO ()
display (word,replacers) | null replacers = return ()
                         | otherwise = print (word,replacers)

main :: IO ()
main = do
    words <- fmap (fromList . lines) (readFile "/usr/share/dict/words")
    mapM_ (display . answers words) (elems words)

-- defeat beat
