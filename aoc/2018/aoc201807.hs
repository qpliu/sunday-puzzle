{-
-- Day 7: The Sum of Its Parts ---

You find yourself standing on a snow-covered coastline; apparently, you landed
a little off course. The region is too hilly to see the North Pole from here,
but you do spot some Elves that seem to be trying to unpack something that
washed ashore. It's quite cold out, so you decide to risk creating a paradox by
asking them for directions.

"Oh, are you the search party?" Somehow, you can understand whatever Elves from
the year 1018 speak; you assume it's Ancient Nordic Elvish. Could the device on
your wrist also be a translator? "Those clothes don't look very warm; take
this." They hand you a heavy coat.

"We do need to find our way back to the North Pole, but we have higher
priorities at the moment. You see, believe it or not, this box contains
something that will solve all of Santa's transportation problems - at least,
that's what it looks like from the pictures in the instructions." It doesn't
seem like they can read whatever language it's in, but you can: "Sleigh kit.
Some assembly required."

"'Sleigh'? What a wonderful name! You must help us assemble this 'sleigh' at
once!" They start excitedly pulling more parts out of the box.

The instructions specify a series of steps and requirements about which steps
must be finished before others can begin (your puzzle input). Each step is
designated by a single letter. For example, suppose you have the following
instructions:

| Step C must be finished before step A can begin.
| Step C must be finished before step F can begin.
| Step A must be finished before step B can begin.
| Step A must be finished before step D can begin.
| Step B must be finished before step E can begin.
| Step D must be finished before step E can begin.
| Step F must be finished before step E can begin.

Visually, these requirements look like this:

|   -->A--->B--
|  /    \      \
| C      -->D----->E
|  \           /
|   ---->F-----

Your first goal is to determine the order in which the steps should be
completed. If more than one step is ready, choose the step which is first
alphabetically. In this example, the steps would be completed as follows:

 - Only C is available, and so it is done first.
 - Next, both A and F are available. A is first alphabetically, so it is done
   next.
 - Then, even though F was available earlier, steps B and D are now also
   available, and B is the first alphabetically of the three.
 - After that, only D and F are available. E is not available because only some
   of its prerequisites are complete. Therefore, D is completed next.
 - F is the only choice, so it is done next.
 - Finally, E is completed.

So, in this example, the correct order is CABDFE.

In what order should the steps in your instructions be completed?
-}

import Data.Char(ord)
import Data.List(sort)
import Data.Map(Map,alter,findWithDefault)
import qualified Data.Map
import Data.Set(Set,empty,delete,difference,fromList,insert,size,toList)

parse :: String -> (Set Char,Map Char (Set Char))
parse = p (empty,Data.Map.empty) . words
  where
    p (steps,table) ("Step":[prereq]:"must":"be":"finished":"before":"step":[step]:"can":"begin.":rest) = 
        p (insert step (insert prereq steps),alter (Just . maybe (fromList [prereq]) (insert prereq)) step table) rest
    p (steps,table) _ = (steps,table)

next :: Map Char (Set Char) -> Set Char -> Set Char -> [Char]
next prereqs finished unfinished =
    filter available $ toList unfinished
  where
    available x =
        size (findWithDefault empty x prereqs `difference` finished) == 0

steps :: Map Char (Set Char) -> Set Char -> Set Char -> [Char]
steps prereqs finished unfinished
  | size unfinished == 0 = []
  | otherwise =
      step : steps prereqs (insert step finished) (delete step unfinished)
  where step = minimum $ next prereqs finished unfinished

run :: String -> String
run input = steps prereqs empty tasks
  where (tasks,prereqs) = parse input

testData :: String
testData = "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin."

test :: ()
test
  | run testData /= "CABDFE" = error "a"
  | otherwise = ()

part1 :: IO String
part1 = fmap run $ readFile "input/07.txt"

time :: (Int,Int) -> Map Char (Set Char) -> Set Char -> Int
time (nworkers,baseTime) prereqs tasks = pstep 0 [] empty tasks
  where
    pstep :: Int -> [(Int,Char)] -> Set Char -> Set Char -> Int
    pstep t started finished unstarted
      | null started && null available = t
      | length started >= nworkers || null available =
          let ((newT,justFinished):newStarted) = sort started
          in  pstep newT newStarted (insert justFinished finished) unstarted
      | otherwise =
          let step = head available
          in  pstep t ((t + 1 + ord step - ord 'A' + baseTime,step):started) finished (delete step unstarted)
      where available = next prereqs finished unstarted

run2 :: (Int,Int) -> String -> Int
run2 (nworkers,baseTime) input = time (nworkers,baseTime) prereqs tasks
  where (tasks,prereqs) = parse input

test2 :: ()
test2
  | run2 (2,0) testData /= 15 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (run2 (5,60)) $ readFile "input/07.txt"
