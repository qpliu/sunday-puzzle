import Data.Char(toLower)
import Data.List(sort)
import Data.Map(Map,fromList,lookup)
import Prelude hiding (lookup)

entries :: String -> [(String,String)]
entries name = map (flip (,) name . sort . map toLower) (drop1 (filter (/= ' ') name))

drop1 :: [a] -> [[a]]
drop1 as = take n (map (take (n-1)) (iterate (drop (n-1)) (cycle as)))
  where n = length as

caps :: Map String String
caps = fromList (concatMap entries [
    "Honolulu", "Juneau", "Olympia", "Salem", "Sacramento",
    "Carson City", "Boise", "Helena", "Cheyenne", "Salt Lake City",
    "Denver", "Phoenix", "Santa Fe", "Austin", "Oklahoma City",
    "Topeka", "Lincoln", "Pierre", "Bismarck", "Saint Paul",
    "Des Moines", "Jefferson City", "Little Rock", "Baton Rouge", "Jackson",
    "Montgomery", "Atlanta", "Tallahassee", "Columbia", "Raleigh",
    "Madison", "Lansing", "Springfield", "Indianopolis", "Columbus",
    "Nashville", "Frankfort", "Charleston", "Harrisburg", "Richmond",
    "Annapolis", "Dover", "Trenton", "Albany", "Hartford",
    "Providence", "Boston", "Montpelier", "Concord", "Augusta"
  ])

check :: String -> Maybe (String,String)
check city = fmap ((,) city) (lookup ((sort . map toLower . filter (/= ' ')) city) caps)

-- Salem:Ames,Mesa
