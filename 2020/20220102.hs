import Data.Set(Set,empty,insert,member)

op :: String -> String
op (l1:l2:l3:l4:l5:l6:l7:ls) = l6:l5:l7:l1:l2:l3:l4:ls
op _ = ""

iop :: String -> String
iop (l1:l2:l3:l4:l5:l6:l7:ls) = l4:l5:l6:l7:l2:l1:l3:ls
iop _ = ""

search :: Set String -> [String] -> [(String,String)]
search _ [] = []
search set (word:words)
  | null w1 = search set words
  | w1 `member` set && w2 `member` set = (word,w1):(word,w2):search set1 words
  | w1 `member` set = (word,w1):search set1 words
  | w2 `member` set = (word,w2):search set1 words
  | otherwise = search set1 words
  where
    w1 = op word
    w2 = iop word
    set1 = insert word set

extras :: [String]
extras = ["greenonion","redcabbage","radishes","potatoes","tomatoes",
    "bokchoy","sweetpotato","sugarcane","snowpeas","snowpea","sugarbeet",
    "swisschard","seabeet","greenbean","limabean","snappea","splitpea",
    "aubergine","springonion","bellpepper","sweetpepper","cassava",
    "watercress","wildrice","brownrice","whiterice","shiitake","chanterelle",
    "soybean","soyabean","blackbean","pintobean","braindead","comatose",
    "bambooshoot","beansprout","brusselsprout"]

main :: IO ()
main = readFile "/usr/share/dict/words" >>= (mapM_ print . search empty . (extras ++) . lines)

-- ???
