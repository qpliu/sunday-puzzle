import Data.Set(fromList,member)

validPair :: String -> Bool
validPair = flip member (fromList ["ez", "ne", "nv", "nq", "dk", "dq", "bd", "bt", "ad", "at", "ag", "cd", "kg", "sa", "vu", "qd", "qt", "ic", "iv", "ue", "zd", "zt"])

initialValidPairs :: String -> [(String,String)]
initialValidPairs "" = []
initialValidPairs (c:cs) = filter (validPair . snd) (makePairs c cs "")

makePairs :: Char -> String -> String -> [(String,String)]
makePairs ch "" rest = []
makePairs ch (c:cs) rest = (cs++rest,[ch,c]):(cs++rest,[c,ch]):makePairs ch cs (c:rest)

search :: (String,[String]) -> [[String]]
search ("",pairs) = [pairs]
search (str,pairs) = concatMap search (map (fmap (:pairs)) (initialValidPairs str))

main :: IO ()
main = (mapM_ print . fromList . map fromList . search) ("endbackstagetvquiz",[])

-- ["ez","qt","vu","ic","kg","at","sa","bd","ne"]
