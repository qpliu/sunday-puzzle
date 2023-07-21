import Data.List(sortBy)
import Data.Map(Map,fromList,toList,unionWith)
import qualified Data.Map

wins :: Ord a => [a] -> Map a Rational
wins [a,b] = fromList [(a,1/2),(b,1/2)]
wins as = Data.Map.map (/2) $ unionWith (+) (wins (drop 2 as ++ take 1 as)) (wins (drop 3 as ++ take 2 as))

main :: IO ()
main = mapM_ print $ sortBy (\ a b -> compare (snd b) (snd a)) $ toList $ wins [1..20]
