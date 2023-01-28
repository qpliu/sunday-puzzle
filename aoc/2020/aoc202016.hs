{-
--- Day 16: Ticket Translation ---

As you're walking to yet another connecting flight, you realize that one of the
legs of your re-routed trip coming up is on a high-speed train. However, the
train ticket you were given is in a language you don't understand. You should
probably figure out what it says before you get to the train station after the
next flight.

Unfortunately, you can't actually read the words on the ticket. You can,
however, read the numbers, and so you figure out the fields these tickets must
have and the valid ranges for values in those fields.

You collect the rules for ticket fields, the numbers on your ticket, and the
numbers on other nearby tickets for the same train service (via the airport
security cameras) together into a single document you can reference (your
puzzle input).

The rules for ticket fields specify a list of fields that exist somewhere on
the ticket and the valid ranges of values for each field. For example, a rule
like class: 1-3 or 5-7 means that one of the fields in every ticket is named
class and can be any value in the ranges 1-3 or 5-7 (inclusive, such that 3 and
5 are both valid in this field, but 4 is not).

Each ticket is represented by a single line of comma-separated values. The
values are the numbers on the ticket in the order they appear; every ticket has
the same format. For example, consider this ticket:

| .--------------------------------------------------------.
| | ????: 101    ?????: 102   ??????????: 103     ???: 104 |
| |                                                        |
| | ??: 301  ??: 302             ???????: 303      ??????? |
| | ??: 401  ??: 402           ???? ????: 403    ????????? |
| '--------------------------------------------------------'

Here, ? represents text in a language you don't understand. This ticket might
be represented as 101,102,103,104,301,302,303,401,402,403; of course, the
actual train tickets you're looking at are much more complicated. In any case,
you've extracted just the numbers in such a way that the first number is always
the same specific field, the second number is always a different specific
field, and so on - you just don't know what each position actually means!

Start by determining which tickets are completely invalid; these are tickets
that contain values which aren't valid for any field. Ignore your ticket for
now.

For example, suppose you have the following notes:

| class: 1-3 or 5-7
| row: 6-11 or 33-44
| seat: 13-40 or 45-50
| 
| your ticket:
| 7,1,14
| 
| nearby tickets:
| 7,3,47
| 40,4,50
| 55,2,20
| 38,6,12

It doesn't matter which position corresponds to which field; you can identify
invalid nearby tickets by considering only whether tickets contain values that
are not valid for any field. In this example, the values on the first nearby
ticket are all valid for at least one field. This is not true of the other
three nearby tickets: the values 4, 55, and 12 are are not valid for any field.
Adding together all of the invalid values produces your ticket scanning error
rate: 4 + 55 + 12 = 71.

Consider the validity of the nearby tickets you scanned. What is your ticket
scanning error rate?
-}

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
