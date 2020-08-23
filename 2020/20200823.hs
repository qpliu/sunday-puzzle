import Data.Char(isAlpha)
import Data.Set(fromList,member)

w :: String -> String -> Int -> (Char -> Char) -> [String]
w revhead remaining n alter
  | n <= 0 = [reverse revhead ++ remaining]
  | null remaining = []
  | not (isAlpha (alter (head remaining))) = w (head remaining:revhead) (tail remaining) n alter
  | otherwise = w (head remaining:revhead) (tail remaining) n alter ++ w (alter (head remaining):revhead) (tail remaining) (n-1) alter

w4 :: String -> [String]
w4 word3 = w [] word3 3 succ

w3 :: String -> [String]
w3 word4 = w [] word4 3 pred

t :: String -> ([String],[String])
t word = (w3 word,w4 word)

test :: IO ()
test = interact (unlines. map (show . t) . lines)

main :: IO ()
main = do
    words <- fmap (fromList . lines) (readFile "/usr/share/dict/words")
    mapM_ (printPairs words) words
  where
    printPairs words word = mapM_ (p words word) (w4 word)
    p words word w | w `member` words = print (word,w) | otherwise = return ()

-- good hope
