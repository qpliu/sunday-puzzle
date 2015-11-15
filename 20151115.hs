import Data.Set(Set,elems,fromList,member)

replacements :: String -> Maybe [String]
replacements word = rep "" word
  where
    rep pre (a:b:c:rest) | succ a == b && succ b == c = reps (reverse pre) rest
                         | otherwise = rep (a:pre) (b:c:rest)
    rep _ _ = Nothing
    reps pre post = Just (map ((pre++) . (:post)) ['a'..'z'])

answers :: Set String -> String -> Maybe (String,[String])
answers words word =
    fmap ((,) word . filter (`member` words)) (replacements word)

display :: Maybe (String,[String]) -> IO ()
display Nothing = return ()
display (Just (word,replacers)) | null replacers = return ()
                                 | otherwise = print (word,replacers)

main :: IO ()
main = do
    words <- fmap (fromList . lines) (readFile "/usr/share/dict/words")
    mapM_ (display . answers words) (elems words)

-- defeat beat
