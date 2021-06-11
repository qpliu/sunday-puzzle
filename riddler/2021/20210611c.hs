import Data.Map(Map,delete,empty,fold,fromList,insert,member,(!))
import qualified Data.Map

data State = State String Int [String]

states :: Map String State
states = fromList [
    ("AL", State "AL" 52420 ["TN","GA","FL","MS"]),
    ("AK", State "AK" 665384 []),
    ("AZ", State "AZ" 113990 ["UT","NM","CA","NV"]),
    ("AR", State "AR" 53179 ["MO","TN","MS","LA","TX","OK"]),
    ("CA", State "CA" 163695 ["OR","NV","AZ"]),
    ("CO", State "CO" 104094 ["WY","NE","KS","OK","NM","UT"]),
    ("CT", State "CT" 5543 ["MA","RI","NY"]),
    ("DE", State "DE" 2489 ["PA","NJ","MD"]),
    ("DC", State "DC" 68 ["MD","VA"]),
    ("FL", State "FL" 65758 ["GA","AL"]),
    ("GA", State "GA" 59425 ["TN","NC","SC","FL","AL"]),
    ("HI", State "HI" 10932 []),
    ("ID", State "ID" 83569 ["MT","WY","UT","NV","OR","WA"]),
    ("IL", State "IL" 57914 ["WI","IN","KY","MO","IA"]),
    ("IN", State "IN" 36420 ["MI-LP","OH","KY","IL"]),
    ("IA", State "IA" 56273 ["MN","WI","IL","MO","NE","SD"]),
    ("KS", State "KS" 82278 ["NE","MO","OK","CO"]),
    ("KY", State "KY" 40408 ["IN","OH","WV","VA","TN","MO","IL"]),
    ("LA", State "LA" 52378 ["AR","MS","TX"]),
    ("ME", State "ME" 35380 ["NH"]),
    ("MD", State "MD" 12406 ["PA","DE","VA","DC","WV"]),
    ("MA", State "MA" 10554 ["VT","NH","RI","CT","NY"]),
    ("MI", State "MI" 96714 []),
    ("MI-LP", State "MI-LP" 80337 ["OH","IN"]),
    ("MI-UP", State "MI-UP" 16377 ["WI"]),
    ("MN", State "MN" 86936 ["WI","IA","SD","ND"]),
    ("MS", State "MS" 48432 ["TN","AL","LA","AR"]),
    ("MO", State "MO" 69707 ["IA","IL","KY","TN","AR","OK","KS","NE"]),
    ("MT", State "MT" 147040 ["ND","SD","WY","ID"]),
    ("NE", State "NE" 77348 ["SD","IA","MO","KS","CO","WY"]),
    ("NV", State "NV" 110572 ["ID","UT","AZ","CA","OR"]),
    ("NH", State "NH" 9349 ["ME","MA","VT"]),
    ("NJ", State "NJ" 8723 ["NY","DE","PA"]),
    ("NM", State "NM" 121590 ["CO","OK","TX","AZ"]),
    ("NY", State "NY" 54555 ["VT","MA","CT","NJ","PA"]),
    ("NC", State "NC" 53819 ["VA","SC","GA","TN"]),
    ("ND", State "ND" 70698 ["MN","SD","MT"]),
    ("OH", State "OH" 44826 ["MI-LP","PA","WV","KY","IN"]),
    ("OK", State "OK" 69899 ["KS","MO","AR","TX","NM","CO"]),
    ("OR", State "OR" 98379 ["WA","ID","NV","CA"]),
    ("PA", State "PA" 46054 ["NY","NJ","DE","MD","WV","OH"]),
    ("RI", State "RI" 1545 ["MA","CT"]),
    ("SC", State "SC" 32020 ["NC","GA"]),
    ("SD", State "SD" 77116 ["ND","MN","IA","NE","WY","MT"]),
    ("TN", State "TN" 42144 ["KY","VA","NC","GA","AL","MS","AR","MO"]),
    ("TX", State "TX" 268596 ["OK","AR","LA","NM"]),
    ("UT", State "UT" 84897 ["ID","WY","CO","AZ","NV"]),
    ("VT", State "VT" 9616 ["NH","MA","NY"]),
    ("VA", State "VA" 42775 ["MD","DC","NC","TN","KY","WV"]),
    ("WA", State "WA" 71298 ["ID","OR"]),
    ("WV", State "WV" 24230 ["OH","PA","MD","VA","KY"]),
    ("WI", State "WI" 65496 ["MI-UP","IL","IA","MN"]),
    ("WY", State "WY" 97813 ["MT","SD","NE","CO","UT","ID"])]

checkGraph :: Bool
checkGraph = all ok states
  where ok (State st _ borders) = all (has st) (map (states!) borders)
        has st (State st2 _ borders) = st `elem` borders

area :: Map a State -> Int
area states = fold addArea 0 states
  where addArea (State _ area _) total = area + total

contiguousFrom :: Map String State -> String -> Map String State
contiguousFrom states state = walk empty (states!state)
  where walk contig s@(State st _ borders)
         | st `member` contig = contig
         | otherwise = foldl walk (insert st s contig) (map (states!) borders)

test :: [String] -> (Int,Int,Int)
test removals = (west,east,(10000*min east west) `div` t48)
  where
    west = area (contiguousFrom newStates "WA")
    east = area (contiguousFrom newStates "FL")
    t48 = area (contiguousFrom states "WA")
    newStates = Data.Map.map rm (foldr delete states removals)
    rm (State st area borders) = State st area (filter (not . (`elem` removals)) borders)

main :: IO ()
main = do
    print (test ["IL","MO","OK","NM"])
    print (test ["MN","IA","MO","OK","NM"])
