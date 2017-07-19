import Data.Char(toLower)
import Data.Maybe(catMaybes)

states :: [String]
states = (filter ((< 12) . length) . map (filter (/= ' ') . map toLower)) [
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
  "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
  "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
  "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
  "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
  "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
  "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ]

novels :: [String]
novels = map (filter (/= ' ') . map toLower) [
  "Anna Karenina", "Madame Bovary", "Patriot Games", "The Outsiders",
  "Tortilla Flat", "Invisible Man", "Eugene Onegin"
  ]

test :: String -> String -> Maybe String
test novel state
    | (length . filter id) (zipWith (/=) vel state) == 2 =
        Just (no ++ " " ++ state)
    | otherwise = Nothing
  where
    (no,vel) = splitAt (length novel - length state) novel

main :: IO ()
main = (mapM_ print . catMaybes) [test n s | n <- novels, s <- states]

-- Eugene Onegin Eugene Oregon
