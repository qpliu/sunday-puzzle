import Data.List(sort)
import Data.Map(Map,alter,elems,empty)

possible :: String -> Bool
possible word
  | length word /= 5 = False
  | length (filter (`elem` "aeiouy") word) < 3 = False
  | otherwise = True

collect :: [String] -> [[String]]
collect words = elems (foldl add empty words)
  where
    add m word
      | possible word = alter (Just . (word:) . maybe [] id) (sort word) m
      | otherwise = m

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ (putStrLn . show) . filter ((>= 3) . length) . collect . lines

count :: IO ()
count = readFile "/usr/share/dict/words" >>= putStrLn . show . length . filter ((>= 3) . length) . collect . lines

-- yaird? dairy diary
-- laine? anile? alien
