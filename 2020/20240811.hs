import Data.Char(toLower)

scan :: (String,Bool,[String]) -> [String] -> [(String,[String])]
scan _ [] = []
scan collected@(four,have5,six) (word:words)
  | have5 && word4 /= four && six /= [] = (four ++ "k",six) : scan next5 words
  | word4 /= four = scan next5 words
  | otherwise = scan next4 words
  where
    word4 = take 4 word
    next4
      | drop 4 word == "k" = (four,True,six)
      | length word == 6 = (four,have5,word:six)
      | otherwise = collected
    next5
      | drop 4 word == "k" = (word4,True,[])
      | length word == 6 = (word4,False,[word])
      | otherwise = (word4,False,[])

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . scan ("",False,[]) . words . map toLower

-- quick quiche
