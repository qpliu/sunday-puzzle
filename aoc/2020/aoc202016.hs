import Data.Array(inRange)
import Data.Map(Map,mapWithKey,toList)
import qualified Data.Map
import Data.Set(Set,difference,size)
import qualified Data.Set

type Field = (String,[(Int,Int)])
type Ticket = [Int]

parse :: String -> ([Field],Ticket,[Ticket])
parse = parseFields [] . lines
  where
    parseFields fields ("":"your ticket:":ticket:"":"nearby tickets:":rest) =
        (fields,parseTicket ticket,map parseTicket rest)
    parseFields fields (field:rest) =
        parseFields (parseField field:fields) rest
    parseField field = (name,parseRanges ("or":words (drop 2 ranges)))
      where (name,ranges) = span (/= ':') field
    parseRanges ("or":range:rest) = parseRange range : parseRanges rest
    parseRanges [] = []
    parseRange range = read ("("++map toComma range++")")
      where toComma c | c == '-' = ',' | otherwise = c
    parseTicket ticket = read ('[':ticket ++ "]")

valid :: Field -> Int -> Bool
valid (_,ranges) value = any (`inRange` value) ranges

completelyInvalid :: [Field] -> Ticket -> [Int]
completelyInvalid fields ticket = filter invalidForAll ticket
  where
    invalidForAll value = not $ any (`valid` value) fields

testData :: String
testData = "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12\n"

test :: ()
test
  | (sum . concatMap (completelyInvalid fields)) tickets /= 71 = error "a"
  | otherwise = ()
  where
    (fields,_,tickets) = parse testData

part1 :: IO Int
part1 = do
    (fields,myTicket,tickets) <- fmap parse $ readFile "input/16.txt"
    return $ sum $ concatMap (completelyInvalid fields) tickets

run2 :: ([Field],Ticket,[Ticket]) -> Int
run2 (fields,myTicket,allTickets) = product $ map (myTicket!!) finalCols
  where
    tickets = filter (null . completelyInvalid fields) allTickets
    cols = [0..length myTicket - 1]
    names = map fst fields

    initPossibleCols :: Map String (Set Int)
    initPossibleCols = Data.Map.fromList $ map possibleCols fields

    finalCols :: [Int]
    finalCols = map (minimum . snd) $ filter ((== "departure") . take 9 . fst) $ toList $ reduce initPossibleCols

    possibleCols :: Field -> (String,Set Int)
    possibleCols field@(name,_) =
        (name,Data.Set.fromList $ filter possible cols)
      where
        possible col = all (valid field) (map (!!col) tickets)

    reduce :: Map String (Set Int) -> Map String (Set Int)
    reduce possibles
      | reduced == possibles = possibles
      | otherwise = reduce reduced
      where
        singles :: [(String,Set Int)]
        singles = filter ((== 1) . size . snd) $ toList possibles
        reduced :: Map String (Set Int)
        reduced = foldr reduce1 possibles singles
        reduce1 :: (String,Set Int) -> Map String (Set Int) -> Map String (Set Int)
        reduce1 single@(name,col) possibs =
          mapWithKey (\ n cols -> if n == name then cols else difference cols col) possibs

part2 :: IO Int
part2 = fmap (run2 . parse) $ readFile "input/16.txt"
