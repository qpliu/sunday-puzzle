import Data.Char(ord,toLower)

scan :: String -> [String] -> [String] -> [(String,[String])]
scan _ _ [] = []
scan prefix w5s (word:words)
  | take 3 w /= prefix = scan (take 3 w) (if is5 then [word] else []) words
  | is5 = scan prefix (word:w5s) words
  | is4 && not (null w5s) = (word,w5s):scan "" [] words
  | otherwise = scan prefix w5s words
  where
    w = map toLower word
    is5 = sum (map ord w) == 5*ord 'a' - 5 + 51
    is4 = sum (map ord w) == 4*ord 'a' - 4 + 51


main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . scan "" [] . lines

-- thick thin
