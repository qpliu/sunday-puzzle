import Data.Char(isLower)
import Data.List(sort,subsequences)
import Data.Map(Map,alter,empty,member,toList,(!))

collect :: (Map String [String],[String]) -> [String] -> (Map String [String],[String])
collect (m,nines) [] = (m,nines)
collect (m,nines) (w:ws)
  | length w == 9 = collect (m,w:nines) ws
  | length w `elem` [2..5] = collect (alter (Just . maybe [w] (w:)) (sort w) m,nines) ws
  | otherwise = collect (m,nines) ws

search :: Map String [String] -> [String] -> [(String,[([String],[String])])]
search _ [] = []
search m (w@[a,_,c,d,e,_,g,h,i]:ws)
  | splits == [] = search m ws
  | otherwise = (w,splits) : search m ws
  where
    letters = sort [a,c,d,e,g,h,i]
    splits = check (subsequences letters)
    complement as [] = as
    complement [] bs = []
    complement (a:as) (b:bs)
      | a == b = complement as bs
      | a > b = complement (a:as) bs
      | otherwise = a : complement as (b:bs)
    check [] = []
    check (k:ks)
      | k `member` m && l `member` m = (m!k,m!l) : check ks
      | otherwise = check ks
      where l = complement letters k

main :: IO ()
main = readFile "/usr/share/dict/words" >>= mapM_ print . uncurry search . collect (empty,[]) . filter (all isLower) . words

-- plowshare reap sow
