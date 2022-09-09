import Data.Map(Map,fromList,(!))

count :: (Int,Int) -> Integer
count (soldiers,castles) = counts!(soldiers,castles)
  where
    counts = fromList [((s,c),n s c) | s <- [0..soldiers], c <- [0..castles]]
    n s c | s == 0 || c == 1 = 1
          | s < 0 || c <= 0 = error "error"
          | otherwise = sum [counts!(i,c-1) | i <- [0..s]]

main :: IO ()
main = print (count (100,10))
