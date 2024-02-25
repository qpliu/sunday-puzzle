import Data.Char(isLower)

genie :: String -> [String]
genie word = [word ++ word ++ "genie",
              word ++ "g" ++ word ++ "enie",
              word ++ "ge" ++ word ++ "nie",
              word ++ "gen" ++ word ++ "ie",
              word ++ "geni" ++ word ++ "e",
              word ++ "genie" ++ word,
              "g" ++ word ++ word ++ "enie",
              "g" ++ word ++ "e" ++ word ++ "nie",
              "g" ++ word ++ "en" ++ word ++ "ie",
              "g" ++ word ++ "eni" ++ word ++ "e",
              "g" ++ word ++ "enie" ++ word,
              "ge" ++ word ++ word ++ "nie",
              "ge" ++ word ++ "n" ++ word ++ "ie",
              "ge" ++ word ++ "ni" ++ word ++ "e",
              "ge" ++ word ++ "ni" ++ word,
              "gen" ++ word ++ word ++ "ie",
              "gen" ++ word ++ "i" ++ word ++ "e",
              "gen" ++ word ++ "ie" ++ word,
              "geni" ++ word ++ word ++ "e",
              "geni" ++ word ++ "e" ++ word,
              "genie" ++ word ++ word]

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ (print . genie) . filter (all isLower) . filter ((== 3) . length) . words

-- golden oldie
