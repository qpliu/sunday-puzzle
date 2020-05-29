import Data.Char(toLower,isAlpha)
import Data.Foldable(maximumBy)
import Data.Map(Map,alter,empty,toList)
import Data.Set(Set,fromList,intersection,member)
import qualified Data.Set

states :: [(String,Set Char)]
states = map f ["Alaska", "Hawaii", "Washington", "Oregon", "California",
       "Nevada", "Idaho", "Montana", "Wyoming", "Utah", "Colorado", "Arizona",
       "New Mexico", "Texas", "Oklahoma", "Kansas", "Nebraska", "South Dakota",
       "North Dakota", "Minnesota", "Iowa", "Missouri", "Arkansas",
       "Louisiana", "Missisippi", "Alabama", "Georgia", "Florida",
       "South Carolina", "North Carolina", "Virginia", "West Virginia",
       "Tennessee", "Kentucky", "Wisconsin", "Michigan", "Illinois", "Indiana",
       "Ohio", "Pennsylvania", "Maryland", "Delaware", "New Jersey",
       "New York", "Connecticut", "Rhode Island", "Massachusetts", "Vermont",
       "New Hampshire", "Maine"]
  where f s = (s,(fromList . map toLower . filter isAlpha) s)

noShareds :: String -> [String]
noShareds word = (map fst . filter noShared) states
  where
    letters = (fromList . map toLower . filter isAlpha) word
    noShared (_,state) = Data.Set.null (intersection letters state)

mackerel :: String -> Bool
mackerel word = length (noShareds word) == 1

collect :: Map String Int -> String -> Map String Int
collect counts word
  | mackerel word = alter (Just . maybe 1 (+ 1)) (head (noShareds word)) counts
  | otherwise = counts

main :: IO ()
main = do
  readFile "word.list" >>= print . maximum . map (\ w -> (length w,w,head (noShareds w))) . filter mackerel . filter ((> 20) . length) . lines
  readFile "word.list" >>= print . maximumBy (\ a b -> compare (snd a) (snd b)) . toList . foldl collect empty . lines

  putStr "Correction: "
  readFile "word.list" >>= print . map (\ w -> (length w,w,head (noShareds w))) . filter mackerel . filter ((> 22) . length) . lines
