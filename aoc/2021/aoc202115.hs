import Debug.Trace(traceShow)

import Data.List(sort)
import Data.Map(Map,delete,empty,findWithDefault,fromList,insert,keysSet,member,minViewWithKey,union,(!))
import qualified Data.Map

parse :: String -> Map (Int,Int) Int
parse = fromList . p 0 0
  where
    p x y ('\n':rest) = p 0 (y+1) rest
    p x y (c:rest) = ((x,y),read [c]) : p (x+1) y rest
    p x y _ = []

-- Depth-first search
-- This is really slow
search :: Map (Int,Int) Int -> (Maybe Int,Map (Int,Int) Int) -> (Int,(Int,Int)) -> (Maybe Int,Map (Int,Int) Int)
search risks (bound,visited) (risk,xy@(x,y))
  | maybe False (newRisk >=) bound = (bound,visited)
  | xy == maximum (keysSet risks) = (Just newRisk,visited)
  | maybe False (newRisk >=) (Data.Map.lookup xy visited) = (bound,visited)
  | otherwise = foldl (search risks) (bound,insert xy newRisk visited) nexts
  where
    newRisk = risk + risks!xy
    nexts = map snd $ sort [((-newX-newY,risks!newXY),(newRisk,newXY)) | newXY@(newX,newY) <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)], member newXY risks]

onMap1 :: Map (Int,Int) Int -> (Int,Int) -> Bool
onMap1 = flip member

getRisk1 :: Map (Int,Int) Int -> (Int,Int) -> Int
getRisk1 = (!)

getGoal1 :: Map (Int,Int) Int -> (Int,Int)
getGoal1 = maximum . keysSet

run :: String -> Int
run input = minRisk
  where
    risks = parse input
    (Just minRisk,_) = search risks (Nothing,empty) (-risks!(0,0),(0,0))

testData :: String
testData = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581\n"

test :: ()
test
  | run testData /= 40 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap run $ readFile "input/15.txt"

onMap2 :: Map (Int,Int) Int -> (Int,Int) -> Bool
onMap2 risks (x,y) = x >= 0 && x < 5*(xmax+1) && y >= 0 && y < 5*(ymax+1)
  where (xmax,ymax) = maximum $ keysSet risks

getRisk2 :: Map (Int,Int) Int -> (Int,Int) -> Int
getRisk2 risks (x,y)
  | not (onMap2 risks (x,y)) = error (show (x,y))
  | otherwise = 1 + (risks!(x `mod` xsize,y `mod` ysize) - 1 + x `div` xsize + y `div` ysize) `mod` 9
  where
    (xmax,ymax) = maximum $ keysSet risks
    (xsize,ysize) = (xmax+1,ymax+1)

getGoal2 :: Map (Int,Int) Int -> (Int,Int)
getGoal2 risks = (5*xmax+4,5*ymax+4)
  where (xmax,ymax) = maximum $ keysSet risks

-- This is still way too slow for part 2, but it's almost fast enough
-- for part 1.  A depth-first search is probably the wrong approach.

type Best = (Int,Path,Map (Int,Int) Int)
type Path = [((Int,Int),Int)]

search2 :: ((Int,Int) -> Bool,(Int,Int) -> Int,(Int,Int)) -> Path -> (Best,Map (Int,Int) Int) -> (Best,Map (Int,Int) Int)
search2 risks@(onMap,getRisk,goal) ((xy@(x,y),risk):path) (best@(bestRisk,bestPath,bestPoints),visited)
  | newRisk >= bestRisk = (best,visited)
  | maybe False (newRisk >=) (Data.Map.lookup xy visited) = (best,visited)
  | xy == goal = traceShow ("goal",xy,newRisk,take 5 path) ((newRisk,path,fromList path),visited)
  | otherwise = foldr (search2 risks) nextBestVisited nextPaths
  where
    newRisk = risk + getRisk xy
    nextPaths = map ((:(xy,newRisk):path) . snd) $ sort [((newX+newY,-getRisk newXY),(newXY,newRisk)) | newXY@(newX,newY) <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)], onMap newXY]
    nextBestVisited
      | not (member xy bestPoints) = (best,insert xy newRisk visited)
      | otherwise = traceShow ("imp",xy,improvement,bestRisk+improvement) ((bestRisk+improvement,newBestPath++(xy,newRisk):path,newBestPoints),union {- elems in 1st arg override elems in 2nd arg -} newBestVisited visited)
        where
          improvement = newRisk - bestPoints!xy
          newBestPath = map (fmap (+improvement)) $ takeWhile ((/= xy) . fst) bestPath
          newBestVisited = fromList ((xy,newRisk):newBestPath)
          newBestPoints = fromList (newBestPath ++ (xy,newRisk):path)

run2small :: String -> Int
run2small input = minRisk
  where
    risks = parse input
    ((minRisk,_,_),_) = search2 (onMap1 risks,getRisk1 risks,getGoal1 risks) [((0,0),-risks!(0,0))] ((sum risks,[],empty),empty)

run2 :: String -> Int
run2 input = minRisk
  where
    risks = parse input
    ((minRisk,_,_),_) = search2 (onMap2 risks,getRisk2 risks,getGoal2 risks) [((0,0),-risks!(0,0))] ((25*sum risks,[],empty),empty)

test2 :: ()
test2
  | run2small testData /= 40 = error "a"
  | run2 testData /= 315 = error "b"
  | otherwise = ()

-- https://en.wikipedia.org/wiki/A*_search_algorithm
-- Still too slow, but fast enough to get the answer.
search3 :: ((Int,Int) -> Bool,(Int,Int) -> Int,(Int,Int)) -> (Int,Int) -> Int
search3 (onMap,getRisk,goal@(goalX,goalY)) start@(startX,startY) =
    astar (fromList [((h start,start),())],fromList [(start,h start)],fromList [(start,0)])
  where
    h :: (Int,Int) -> Int
    h xy@(x,y) = abs (x-goalX) + abs (y-goalY)

    astar :: (Map (Int,(Int,Int)) (),Map (Int,Int) Int,Map (Int,Int) Int) -> Int
    astar (openQ,openSet,scores)
      | (Data.Map.size openQ) `mod` 100 == 0 && traceShow (Data.Map.size openQ,xy) False = undefined
      | xy == goal = currentScore
      | otherwise = astar $ foldr (handleNeighbor currentScore) (newOpenQ,newOpenSet,scores) [newXY | newXY <- [(x-1,y),(x,y-1),(x+1,y),(x,y+1)], onMap newXY]
      where
        Just (((_,xy@(x,y)),()),newOpenQ) = minViewWithKey openQ
        newOpenSet = delete xy openSet
        currentScore = scores!xy

    handleNeighbor :: Int -> (Int,Int) -> (Map (Int,(Int,Int)) (),Map (Int,Int) Int,Map (Int,Int) Int) -> (Map (Int,(Int,Int)) (),Map (Int,Int) Int,Map (Int,Int) Int)
    handleNeighbor currentScore xy (openQ,openSet,scores)
      | maybe False ((currentScore+getRisk xy)>=) $ Data.Map.lookup xy scores = (openQ,openSet,scores)
      | member xy openSet = (insert ((currentScore+h xy),xy) () $ delete (openSet!xy,xy) openQ,insert xy (currentScore+h xy) openSet,insert xy (currentScore+getRisk xy) scores)
      | otherwise = (insert ((currentScore+h xy),xy) () openQ,insert xy (currentScore+h xy) openSet,insert xy (currentScore+getRisk xy) scores)

run3small :: String -> Int
run3small input = search3 (onMap1 risks,getRisk1 risks,getGoal1 risks) (0,0)
  where risks = parse input

run3 :: String -> Int
run3 input = search3 (onMap2 risks,getRisk2 risks,getGoal2 risks) (0,0)
  where risks = parse input

test3 :: ()
test3
  | run3small testData /= 40 = error "a"
  | run3 testData /= 315 = error "b"
  | otherwise = ()

part2small :: IO Int
part2small = fmap run3small $ readFile "input/15.txt"

part2 :: IO Int
part2 = fmap run3 $ readFile "input/15.txt"
