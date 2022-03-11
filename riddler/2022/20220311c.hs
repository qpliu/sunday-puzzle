import Data.Map(Map,alter,empty,foldlWithKey,toList)
import Data.Set(Set,fromList,intersection)
import qualified Data.Set

histogram :: Ord k => [(k,Int)] -> Map k Int
histogram items = foldl collect empty items
  where collect hist (k,n) = alter (Just . maybe n (n+)) k hist

wins :: Ord a => Set a -> Map (Set a) Int -> Int
wins numbers hist = foldlWithKey collect 0 hist
  where
    collect count k n | Data.Set.null (intersection k numbers) = count
                      | otherwise = count + n

main :: IO ()
main = do
  let hist = histogram [(fromList [a+b,a+c,a+d,b+c,b+d,c+d],1) | a <- [1..6], b <- [1..6], c <- [1..6], d <- [1..6]]
  print $ maximum [let numbers = fromList [a,b,c,d] in (wins numbers hist,numbers) | a <- [2..12], b <- [2..12], b > a, c <- [2..12], c > b, d <- [2..12], d > c]
