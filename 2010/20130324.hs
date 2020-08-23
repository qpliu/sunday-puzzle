import Data.Set(fromList,member)

valid :: Char -> String -> Bool
valid middle word@[_,_,letter,_,_] =
    letter == middle && not (exclude word) && not (any (`elem` "eiou") word)
valid _ _ = False

exclude :: String -> Bool
exclude = flip member (fromList ["abaca","abask","acana","acara","adays","adlay","afara","agama","agamy","agaty","ajaja","ajava","akala","alada","alala","alary","amaga","amala","amapa","anama","anana","araba","araca","arara","asana","ataxy","awald","awalt","banat","banda","braca","braza","clava","crawm","danda","fanam","ganam","glaga","grama","kanap","kanat","kraal","krama","lanas","malar","malax","malty","manas","palar","palas","palay","panax","pylar","salal","salar","salat","salay","salpa","salta","salty","sanct","scall","scawl","spall","talak","talar","talky","talma","tanka","tansy","trama","wandy","wanga","wanty","xylan","xylyl","yalla","yanky","Banda","Ganda","Nanda","Sandy","Sancy","Tandy","Vanda"])

collect :: [String] -> ([String],[String],[String])
collect = foldl collect' ([],[],[])
  where
    collect' (xxNxx,xxAxx,xxLxx) word
      | valid 'n' word = (word:xxNxx,xxAxx,xxLxx)
      | valid 'a' word = (xxNxx,word:xxAxx,xxLxx)
      | valid 'l' word = (xxNxx,xxAxx,word:xxLxx)
      | otherwise = (xxNxx,xxAxx,xxLxx)

main :: IO ()
main = do
    (n,a,l) <- fmap (collect . lines) (readFile "/usr/share/dict/words")
    print [w1 ++ "/" ++ w2 ++ "/nasal/" ++ w4 ++ "/" ++ w5 | w1 <- n, w2 <- a, m12 w1 w2, w4 <- a, m14 w1 w4, m24 w2 w4, w5 <- l, m15 w1 w5, m25 w2 w5, m45 w4 w5]
  where
    m12 [_,l12,_,_,_] [l21,_,_,_,_] = l12 == l21
    m14 [_,_,_,l14,_] [l41,_,_,_,_] = l14 == l41
    m24 [_,_,_,l24,_] [_,l42,_,_,_] = l24 == l42
    m15 [_,_,_,_,l15] [l51,_,_,_,_] = l15 == l51
    m25 [_,_,_,_,l25] [_,l52,_,_,_] = l25 == l52
    m45 [_,_,_,_,l45] [_,_,_,l54,_] = l45 == l54

-- PANDA
-- APART
-- NASAL
-- DRAMA
-- ATLAS
