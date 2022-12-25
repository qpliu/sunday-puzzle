{-
--- Day 13: Knights of the Dinner Table ---

In years past, the holiday feast with your family hasn't gone so well. Not
everyone gets along! This year, you resolve, will be different. You're going to
find the optimal seating arrangement and avoid all those awkward conversations.

You start by writing up a list of everyone invited and the amount their
happiness would increase or decrease if they were to find themselves sitting
next to each other person. You have a circular table that will be just big
enough to fit everyone comfortably, and so each person will have exactly two
neighbors.

For example, suppose you have only four attendees planned, and you calculate
their potential happiness as follows:

Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.

Then, if you seat Alice next to David, Alice would lose 2 happiness units
(because David talks so much), but David would gain 46 happiness units (because
Alice is such a good listener), for a total change of 44.

If you continue around the table, you could then seat Bob next to Alice (Bob
gains 83, Alice gains 54). Finally, seat Carol, who sits next to Bob (Carol
gains 60, Bob loses 7) and David (Carol gains 55, David gains 41). The
arrangement looks like this:

     +41 +46
+55   David    -2
Carol       Alice
+60    Bob    +54
     -7  +83

After trying every other seating arrangement in this hypothetical scenario, you
find that this one is the most optimal, with a total change in happiness of
330.

What is the total change in happiness for the optimal seating arrangement of the actual guest list?
-}
import Data.List(permutations)
import Data.Map(Map,(!))
import qualified Data.Map
import Data.Set(Set,toList)
import qualified Data.Set

collect :: (Set String,Map (String,String) Int) -> String -> (Set String,Map (String,String) Int)
collect (names,happiness) s = (Data.Set.insert name1 (Data.Set.insert name2 names),Data.Map.insert (name1,name2) (sign*units) happiness)
  where
    (name1:_:gain:number:_:_:_:_:_:_:name2dot:_) = words s
    name2 = takeWhile (/= '.') name2dot
    sign | gain == "gain" = 1 | otherwise = -1
    units = read number

totalChange :: Map (String,String) Int -> String -> [String] -> Int
totalChange happiness name1 names = scorePair name1 (last names) + scorePairs name1 names
  where
    scorePair n1 n2 = happiness!(n1,n2) + happiness!(n2,n1)
    scorePairs n1 (n:ns) = scorePair n1 n + scorePairs n ns
    scorePairs _ _ = 0

-- This algorithm might be too slow for the actual input.
optimal :: (Set String,Map (String,String) Int) -> Int
optimal (names,happiness) = maximum $ map (totalChange happiness name1) $ permutations othernames
  where
    (name1:othernames) = toList names

test :: ()
test
  | optimal guests /= 330 = error "a"
  | otherwise = ()
  where
    guests = foldl collect (Data.Set.empty,Data.Map.empty) $ lines "Alice would gain 54 happiness units by sitting next to Bob.\nAlice would lose 79 happiness units by sitting next to Carol.\nAlice would lose 2 happiness units by sitting next to David.\nBob would gain 83 happiness units by sitting next to Alice.\nBob would lose 7 happiness units by sitting next to Carol.\nBob would lose 63 happiness units by sitting next to David.\nCarol would lose 62 happiness units by sitting next to Alice.\nCarol would gain 60 happiness units by sitting next to Bob.\nCarol would gain 55 happiness units by sitting next to David.\nDavid would gain 46 happiness units by sitting next to Alice.\nDavid would lose 7 happiness units by sitting next to Bob.\nDavid would gain 41 happiness units by sitting next to Carol."

-- The input is small enough to use brute-force.
part1 :: IO Int
part1 = fmap (optimal . foldl collect (Data.Set.empty,Data.Map.empty) . lines) (readFile "input/13.txt")

addMe :: (Set String,Map (String,String) Int) -> (Set String,Map (String,String) Int)
addMe (names,happiness) = (Data.Set.insert "" names,Data.Map.fromList (Data.Map.toList happiness ++ concatMap (\ name -> [((name,""),0),(("",name),0)]) (Data.Set.toList names)))

part2 :: IO Int
part2 = fmap (optimal . addMe . foldl collect (Data.Set.empty,Data.Map.empty) . lines) (readFile "input/13.txt")
