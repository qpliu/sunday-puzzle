import Data.Set(Set,empty,fromList,member)

valid :: String -> Bool
valid word = length word == 3 && all (flip elem "beermouth") word

good :: Set String -> String -> Bool
good words word
  | length word == 3 = take 3 word `elem` words
  | length word == 6 = (take 3 (drop 3 word)) `elem` words
  | length word == 7 =
      [word !! 0,word !! 3,word !! 6] `elem` words &&
      [word !! 6,word !! 4,word !! 2] `elem` words
  | length word == 8 = [word !! 1,word !! 4,word !! 7] `elem` words
  | length word == 9 =
      (take 3 (drop 6 word)) `elem` words &&
      [word !! 2,word !! 5,word !! 8] `elem` words &&
      [word !! 0,word !! 4,word !! 8] `elem` words
  | otherwise = True

search :: (String,String) -> (String -> Bool) -> [String]
search (chrs,candidate) ok
  | not (ok candidate) = []
  | null chrs = [candidate]
  | otherwise = concatMap (flip search ok) (nextCandidates chrs "" candidate)

nextCandidates :: String -> String -> String -> [(String,String)]
nextCandidates chrs nextChrs candidate
  | null chrs = []
  | otherwise = (tail chrs ++ nextChrs, candidate ++ [head chrs]) :
                    nextCandidates (tail chrs) (head chrs:nextChrs) candidate

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . search ("beermouth","") . good . fromList . filter valid . lines

-- OHM RUE BET
