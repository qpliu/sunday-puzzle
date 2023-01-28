{-
--- Day 23: Crab Cups ---

The small crab challenges you to a game! The crab is going to mix up some cups,
and you have to predict where they'll end up.

The cups will be arranged in a circle and labeled clockwise (your puzzle
input). For example, if your labeling were 32415, there would be five cups in
the circle; going clockwise around the circle from the first cup, the cups
would be labeled 3, 2, 4, 1, 5, and then back to 3 again.

Before the crab starts, it will designate the first cup in your list as the
current cup. The crab is then going to do 100 moves.

Each move, the crab does the following actions:

 - The crab picks up the three cups that are immediately clockwise of the
   current cup. They are removed from the circle; cup spacing is adjusted as
   necessary to maintain the circle.
 - The crab selects a destination cup: the cup with a label equal to the
   current cup's label minus one. If this would select one of the cups that was
   just picked up, the crab will keep subtracting one until it finds a cup that
   wasn't just picked up. If at any point in this process the value goes below
   the lowest value on any cup's label, it wraps around to the highest value on
   any cup's label instead.
 - The crab places the cups it just picked up so that they are immediately
   clockwise of the destination cup. They keep the same order as when they were
   picked up.
 - The crab selects a new current cup: the cup which is immediately clockwise
   of the current cup.

For example, suppose your cup labeling were 389125467. If the crab were to do
merely 10 moves, the following changes would occur:

| -- move 1 --
| cups: (3) 8  9  1  2  5  4  6  7 
| pick up: 8, 9, 1
| destination: 2
| 
| -- move 2 --
| cups:  3 (2) 8  9  1  5  4  6  7 
| pick up: 8, 9, 1
| destination: 7
| 
| -- move 3 --
| cups:  3  2 (5) 4  6  7  8  9  1 
| pick up: 4, 6, 7
| destination: 3
| 
| -- move 4 --
| cups:  7  2  5 (8) 9  1  3  4  6 
| pick up: 9, 1, 3
| destination: 7
| 
| -- move 5 --
| cups:  3  2  5  8 (4) 6  7  9  1 
| pick up: 6, 7, 9
| destination: 3
| 
| -- move 6 --
| cups:  9  2  5  8  4 (1) 3  6  7 
| pick up: 3, 6, 7
| destination: 9
| 
| -- move 7 --
| cups:  7  2  5  8  4  1 (9) 3  6 
| pick up: 3, 6, 7
| destination: 8
| 
| -- move 8 --
| cups:  8  3  6  7  4  1  9 (2) 5 
| pick up: 5, 8, 3
| destination: 1
| 
| -- move 9 --
| cups:  7  4  1  5  8  3  9  2 (6)
| pick up: 7, 4, 1
| destination: 5
| 
| -- move 10 --
| cups: (5) 7  4  1  8  3  9  2  6 
| pick up: 7, 4, 1
| destination: 3
| 
| -- final --
| cups:  5 (8) 3  7  4  1  9  2  6 

In the above example, the cups' values are the labels as they appear moving
clockwise around the circle; the current cup is marked with ( ).

After the crab is done, what order will the cups be in? Starting after the cup
labeled 1, collect the other cups' labels clockwise into a single string with
no extra characters; each number except 1 should appear exactly once. In the
above example, after 10 moves, the cups clockwise from 1 are labeled 9, 2, 6,
5, and so on, producing 92658374. If the crab were to complete all 100 moves,
the order after cup 1 would be 67384529.

Using your labeling, simulate 100 moves. What are the labels on the cups after
cup 1?

--- Part Two ---

Due to what you can only assume is a mistranslation (you're not exactly fluent
in Crab), you are quite surprised when the crab starts arranging many cups in a
circle on your raft - one million (1000000) in total.

Your labeling is still correct for the first few cups; after that, the
remaining cups are just numbered in an increasing fashion starting from the
number after the highest number in your list and proceeding one by one until
one million is reached. (For example, if your labeling were 54321, the cups
would be numbered 5, 4, 3, 2, 1, and then start counting up from 6 until one
million is reached.) In this way, every number from one through one million is
used exactly once.

After discovering where you made the mistake in translating Crab Numbers, you
realize the small crab isn't going to do merely 100 moves; the crab is going to
do ten million (10000000) moves!

The crab is going to hide your stars - one each - under the two cups that will
end up immediately clockwise of cup 1. You can have them if you predict what
the labels on those cups will be when the crab is finished.

In the above example (389125467), this would be 934001 and then 159792;
multiplying these together produces 149245887792.

Determine which two cups will end up immediately clockwise of cup 1. What do
you get if you multiply their labels together?
-}

import Data.List(elemIndex)
import Data.Map(Map,findWithDefault,fromList,insert)

move :: Ord a => [a] -> [a]
move (a1:a2:a3:a4:rest) = left ++ (dest:a2:a3:a4:tail right) ++ [a1]
  where
    lesser = filter (< a1) rest
    dest | null lesser = maximum rest | otherwise = maximum lesser
    (left,right) = span (/= dest) rest

reorder :: Ord a => [a] -> [a]
reorder as = right ++ left
  where (left,right) = span (/= (minimum as)) as

test :: ()
test
  | (tail . reorder . head . drop 10 . iterate move) "389125467" /= "92658374" = error "a"
  | (tail . reorder . head . drop 100 . iterate move) "389125467" /= "67384529" = error "b"
  | otherwise = ()

part1 :: Ord a => [a] -> [a]
part1 = tail . reorder . head . drop 100 . iterate move

initial2 :: ([Int],Int) -> (Int -> Int,Int -> Int,Int) -- (cup to position,position to cup,max position)
initial2 (list,maxPos) = (cupToPos,posToCup,maxPos)
  where
    cupToPos cup = maybe cup (+1) $ elemIndex cup list
    posToCup pos | pos <= length list = list !! (pos-1) | otherwise = pos

move2 :: (Int -> Int,Int -> Int,Int) ->  (Int -> Int,Int -> Int,Int)
move2 (cupToPos,posToCup,maxPos) = (nextCupToPos,nextPosToCup,maxPos)
  where
    currentCup = posToCup 1
    targetCup = findTargetCup (currentCup - 1)
    targetPos = cupToPos targetCup
    movedCups = map posToCup [2,3,4]

    nextCupToPos cup
      | pos == 1 = maxPos
      | pos == 2 = targetPos - 3
      | pos == 3 = targetPos - 2
      | pos == 4 = targetPos - 1
      | pos <= targetPos = pos - 4
      | otherwise = pos - 1
      where
        pos = cupToPos cup
    nextPosToCup pos
      | pos <= targetPos - 4 = posToCup (pos+4)
      | pos == targetPos - 3 = posToCup 2
      | pos == targetPos - 2 = posToCup 3
      | pos == targetPos - 1 = posToCup 4
      | pos == maxPos = posToCup 1
      | otherwise = posToCup (pos+1)

    findTargetCup target
      | target <= 0 = findTargetCup maxPos
      | target `elem` movedCups = findTargetCup (target-1)
      | otherwise = target

-- Can't figure out a way how to do this for a whole cycle of moves.
doLoop2 :: (Int -> Int,Int -> Int,Int) -> (Int -> Int,Int -> Int,Int)
doLoop2 (cupToPos,posToCup,maxPos) = (nextCupToPos,nextPosToCup,maxPos)
  where
    nextCupToPos cup = undefined
    nextPosToCup pos = undefined

run2 :: ([Int],Int) -> (Int,Int)
run2 (list,maxPos) = (posToCup (pos1+1),posToCup (pos1+2))
  where
    ((cupToPos,posToCup,_):_) = drop 40 $ iterate doLoop2 (initial2 (list,maxPos))
    pos1 = cupToPos 1

-- After internet hint, have to brute-force it, but without doing tons
-- of copies per move.
initial3 :: [Int] -> Int -> (Int,Int,Map Int Int)
initial3 inits len
  | len > length inits =
      (head (inits++[1]),len,fromList $ zip (len:inits) (inits ++ [length inits+1]))
  | otherwise =
      (head inits,length inits,fromList $ zip (last inits:init inits) inits)

move3 :: (Int,Int,Map Int Int) -> (Int,Int,Map Int Int)
move3 (current,len,links) = (nextCurrent,len,nextLinks)
  where
    after n = findWithDefault (n+1) n links
    move1 = after current
    move2 = after move1
    move3 = after move2
    target = findTarget (current-1)

    nextCurrent = after move3
    nextLinks = insert current nextCurrent
              $ insert move3 (after target)
              $ insert target move1 links

    findTarget t
      | t <= 0 = findTarget len
      | t == move1 || t == move2 || t == move3 = findTarget (t-1)
      | otherwise = t

run3 :: [Int] -> Int -> Int -> [Int]
run3 initial len moves = takeWhile (/= 1) $ tail $ iterate after 1
  where
    after n = findWithDefault (n+1) n links
    ((_,_,links):_) = drop moves $ iterate move3 $ initial3 initial len

test2 :: ()
test2
  | take 2 (run3 [3,8,9,1,2,5,4,6,7] 1000000 10000000) /= [934001,159792] = error "a"
  | otherwise = ()

-- Gets stack overflow.  I could learn how to use strictness annotations
-- or just write it in another language.
