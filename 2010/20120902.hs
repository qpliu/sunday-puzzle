import Data.List(nub,sort)
import Data.Map(Map,alter,empty,member,(!))

collect :: Map String [String] -> String -> Map String [String]
collect words word
  | length word /= 6 = words
  | not (all (`elem` "autumnleaves") word) = words
  | word `elem` ["lantum","unlame","amelus","mensal","snavel","avulse","alveus","velate","lateen","lanete","elanet","salten","samlet","unseam","enseam","entame","measle","teasel","stelae","sealet","saltee","eluate","aulete","evalue","valent","levant","lunate","mutase","meatus","sultam","muleta","setula","nasute","setula","unteam","melena","manlet","mantel","sleave"] = words
  | otherwise = alter (maybe (Just [word]) (Just . (word:))) (sort word) words

search :: Map String [String] -> String -> String -> String -> [[String]] -> [[[String]]]
search dict autumn leaves moreleaves chain
  | not (key `member` dict) = []
  | null autumn = [(dict!key):chain]
  | otherwise = concat [search dict autumns (leaf:leaves) moreleaves1 ((dict!key) : chain) | (_,autumns) <- splits autumn, (leaf,moreleaves1) <- splits moreleaves]
  where key = sort ('a':autumn++leaves)

splits :: [a] -> [(a,[a])]
splits as = (map split . take n . flip map [0..] . flip drop . cycle) as
  where
    n = length as
    split as = (head as,tail (take n as))

main :: IO ()
main = do
    dict <- fmap (foldl collect empty . lines) (readFile "/usr/share/dict/words")
    mapM_ (print . reverse) (nub (search dict "utumn" "" "leves" []))
