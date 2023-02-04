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
