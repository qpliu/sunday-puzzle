import Data.List(sort)
import Data.Map(Map,alter,empty)

k6 :: String -> Maybe String
k6 s | length s == 6 = Just (sort s) | otherwise = Nothing

k7 :: String -> Maybe String
k7 ('r':s@[_,_,_,_,_,_]) = Just (sort s)
k7 (l1:'r':s@[_,_,_,_,_]) = Just (sort (l1:s))
k7 (l1:l2:'r':s@[_,_,_,_]) = Just (sort (l1:l2:s))
k7 (l1:l2:l3:'r':s@[_,_,_]) = Just (sort (l1:l2:l3:s))
k7 (l1:l2:l3:l4:'r':s@[_,_]) = Just (sort (l1:l2:l3:l4:s))
k7 (l1:l2:l3:l4:l5:'r':s@[_]) = Just (sort (l1:l2:l3:l4:l5:s))
k7 (l1:l2:l3:l4:l5:l6:'r':[]) = Just (sort (l1:l2:l3:l4:l5:[l6]))
k7 _ = Nothing

add6 :: String -> Map String ([String],[String]) -> String -> Map String ([String],[String])
add6 word m key = alter (Just . maybe ([word],[]) (\ (l6,l7) -> (word:l6,l7))) key m

add7 :: String -> Map String ([String],[String]) -> String -> Map String ([String],[String])
add7 word m key = alter (Just . maybe ([],[word]) (fmap (word:))) key m

collect :: Map String ([String],[String]) -> String -> Map String ([String],[String])
collect m word = c k6 add6 (c k7 add7 m)
  where
    c k a m = maybe m (a word m) (k word)

p :: ([String],[String]) -> IO ()
p ([],_) = return ()
p (_,[]) = return ()
p a = print a

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ p . foldl collect empty . lines

-- tonsil nostril
