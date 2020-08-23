import Data.List(delete)

remove :: String -> String -> Maybe String
remove [] k = Just k
remove _ [] = Nothing
remove (t:ts) k | t `elem` k = remove ts (delete t k) | otherwise = Nothing

solve :: [String] -> String -> Maybe [String]
solve [] _ = Nothing
solve ts k = maybe (solve (tail ts) k) Just (solve' [] ts k)

solve' :: [String] -> [String] -> String -> Maybe [String]
solve' _ _ [] = Nothing
solve' acc _ [_] = Just acc
solve' _ [] _ = Nothing
solve' acc (t:ts) k =
    maybe (solve' acc ts k)
          (maybe (solve' acc ts k) Just . solve' (t:acc) ts) (remove t k)

main :: IO ()
main = print $ solve 
               ["elm", "teak", "cedar", "cherry", "mulberry", "magnolia",
                "sequoia", "redwood", "baobab", "banyan", "juniper", "spruce",
                "willow", "fir", "balsa", "mahogany", "apple", "lemon",
                "palm", "beech", "maple", "yew", "ash", "bonsai", "alder",
                "apricot", "birch", "buckeye", "butternut", "catalpa",
                "chestnut", "dogwood", "ginkgo", "hickory", "holly", "locust",
                "larch", "linden", "mimosa", "osage", "orange", "pecan",
                "peach", "pear", "persimmon", "plum", "poplar", "sassafras",
                "sycamore", "tupelo", "walnut", "cypress", "dogwood",
                "hawthorn", "laurel", "madrone", "nutmeg"]
                ("hemlock" ++ "myrtle" ++ "oak" ++ "pine")
