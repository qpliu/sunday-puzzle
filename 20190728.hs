import Data.Char(chr,ord)

balanced :: String -> Bool
balanced word =
    word == reverse (map (chr . ((ord 'a' + ord 'z') -) . ord) word)

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . filter balanced . filter ((== 6) . length) . lines

-- wizard
