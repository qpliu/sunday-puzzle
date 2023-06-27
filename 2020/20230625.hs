main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ (print . m) . filter f . words
  where
    f word = length word == 5 && last word == 'y'
    m word = (word,'_':drop 1 (reverse word))

-- annoy donna
-- belay caleb
-- silly ellis
-- deray jared
-- agley helga
-- alley bella/della

-- Della Street is the fictional secretary of Perry Mason
