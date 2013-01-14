main = readFile "/usr/share/dict/words" >>= mapM_ putStrLn . filter (flip all "abcdef" . flip elem) . filter ((== 8) . length) . lines
