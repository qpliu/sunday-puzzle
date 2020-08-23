import Data.Char(isLower)
import Data.Set(Set,empty,fromList,member,union)

makeDict :: String -> Set String
makeDict = fromList . filter (all isLower) . filter ((>= 3) . length) . lines

branches :: String -> [String]
branches word = drop1 "" word ++ concatMap (add1 "" word) ['a'..'z']
  where
    drop1 _ [] = []
    drop1 prefix (l:ls) = (prefix ++ ls) : drop1 (prefix ++ [l]) ls
    add1 prefix [] a = [prefix ++ [a]]
    add1 prefix w@(l:ls) a = (prefix ++ [a] ++ w) : add1 (prefix ++ [l]) ls a

validBranches :: Set String -> Set String -> String -> [String]
validBranches dict prune word =
    (filter (flip member dict) . filter (not . flip member prune) . branches) word

search1 :: Set String -> Set String -> [String] -> [String]
search1 dict prune words = concatMap (validBranches dict prune) words

searchFull :: Set String -> String -> [[String]]
searchFull dict word = search' (fromList [word]) [word]
  where search' prune words = next : search' (union prune (fromList next)) next
          where next = concatMap (validBranches dict prune) words

search :: Set String -> String -> Int -> [[String]]
search dict word depth = take depth (searchFull dict word)

runSearch :: String -> Int -> IO [[String]]
runSearch word depth = do
    dict <- fmap makeDict (readFile "/usr/share/dict/words")
    return (search dict word depth)

-- whole hole hoe hoer her hear heart
