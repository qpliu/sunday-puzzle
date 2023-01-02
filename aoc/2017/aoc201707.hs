{-
--- Day 7: Recursive Circus ---

Wandering further through the circuits of the computer, you come upon a tower
of programs that have gotten themselves into a bit of trouble. A recursive
algorithm has gotten out of hand, and now they're balanced precariously in a
large tower.

One program at the bottom supports the entire tower. It's holding a large disc,
and on the disc are balanced several more sub-towers. At the bottom of these
sub-towers, standing on the bottom disc, are other programs, each holding their
own disc, and so on. At the very tops of these sub-sub-sub-...-towers, many
programs stand simply keeping the disc below them balanced but with no disc of
their own.

You offer to help, but first you need to understand the structure of these
towers. You ask each program to yell out their name, their weight, and (if
they're holding a disc) the names of the programs immediately above them
balancing on that disc. You write this information down (your puzzle input).
Unfortunately, in their panic, they don't do this in an orderly fashion; by the
time you're done, you're not sure which program gave which information.

For example, if your list is the following:

| pbga (66)
| xhth (57)
| ebii (61)
| havc (66)
| ktlj (57)
| fwft (72) -> ktlj, cntj, xhth
| qoyq (66)
| padx (45) -> pbga, havc, qoyq
| tknk (41) -> ugml, padx, fwft
| jptl (61)
| ugml (68) -> gyxo, ebii, jptl
| gyxo (61)
| cntj (57)

...then you would be able to recreate the structure of the towers that looks
like this:

|                 gyxo
|               /     
|          ugml - ebii
|        /      \     
|       |         jptl
|       |        
|       |         pbga
|      /        /
| tknk --- padx - havc
|      \        \
|       |         qoyq
|       |             
|       |         ktlj
|        \      /     
|          fwft - cntj
|               \     
|                 xhth

In this example, tknk is at the bottom of the tower (the bottom program), and
is holding up ugml, padx, and fwft. Those programs are, in turn, holding up
other programs; in this example, none of those programs are holding up any
other programs, and are all the tops of their own towers. (The actual tower
balancing in front of you is much larger.)

Before you're ready to help them, you need to make sure your information is
correct. What is the name of the bottom program?
-}

import Data.Char(isDigit)
import Data.List(sort)
import Data.Map(elems,fromList,(!))

data Tower = Tower String Int [Tower] deriving (Eq,Ord,Show)

totalWeight :: Tower -> Int
totalWeight (Tower _ weight subtowers) = weight + sum (map totalWeight subtowers)

towerName :: Tower -> String
towerName (Tower name _ _) = name

parse :: String -> [Tower]
parse s = elems towers
  where
    towers = fromList $ map (parseTower . words) $ lines s
    parseTower [name,weight] = (name,Tower name (read $ filter isDigit weight) [])
    parseTower (name:weight:"->":subtowers) = (name,Tower name (read $ filter isDigit weight) (map ((towers!) . (filter (/= ','))) subtowers))

testData :: [Tower]
testData = parse "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"

findBottom :: [Tower] -> Tower
findBottom towers = snd $ maximum $ zip (map totalWeight towers) towers

test :: ()
test
  | towerName (findBottom testData) /= "tknk" = error "a"
  | otherwise = ()

part1 :: IO String
part1 = fmap (towerName . findBottom . parse) $ readFile "input/07.txt"

balanceWeight :: Tower -> Int
balanceWeight (Tower _ _ towers)
  | length towers < 3 = 0
  | first == second = first
  | otherwise = third
  where (first:second:third:_) = sort (map totalWeight towers)

findUnbalanced :: Int -> Tower -> (Int,String)
findUnbalanced myBalanceWeight tower@(Tower _ weight towers)
  | null towers = error "null towers"
  | null unbalanced = (weight + myBalanceWeight - totalWeight tower,towerName tower)
  | otherwise = findUnbalanced subbalanceWeight (head unbalanced)
  where
    subbalanceWeight = balanceWeight tower
    unbalanced = filter ((/= subbalanceWeight) . totalWeight) towers

test2 :: ()
test2
  | findUnbalanced 0 (findBottom testData) /= (60,"ugml") = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (fst . findUnbalanced 0 . findBottom . parse) $ readFile "input/07.txt"
