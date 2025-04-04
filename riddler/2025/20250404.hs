trios :: Int -> [(Int,Int,Int)]
trios n = [(a,b,c) | a <- [1..n], b <- [a+1..n], c <- [b+1..n]]

canFollow :: (Int,Int,Int) -> (Int,Int,Int) -> Bool
canFollow (a,b,c) (d,e,f) =
    a /= d && a /= e && a /= f
 && b /= d && b /= e && b /= f
 && c /= d && c /= e && c /= f

findValidSequence :: [(Int,Int,Int)] -> [(Int,Int,Int)] -> [(Int,Int,Int)] -> Maybe [(Int,Int,Int)]
findValidSequence seq@(last:_) [] [] = Just seq
findValidSequence seq@(last:_) [] _ = Nothing
findValidSequence seq@(last:_) (next:nexts) invalids
  | canFollow last next =
      maybe (findValidSequence seq nexts (next:invalids)) Just
          $ findValidSequence (next:seq) (nexts++invalids) []
  | otherwise = findValidSequence seq nexts (next:invalids)

find :: Int -> Maybe [(Int,Int,Int)]
find n = findValidSequence [(1,2,3)] (tail (trios n)) []

main :: IO ()
main = mapM_ print $ reverse seq where Just seq = find 7
