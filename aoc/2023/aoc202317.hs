import Debug.Trace(traceShow)

import Data.Char(ord)
import Data.Map(Map,empty,fromList,member,toList,(!))
import qualified Data.Map
import Data.Set(Set,insert,minView,singleton,size)
import qualified Data.Set

parse :: String -> Map (Int,Int) Int
parse = Data.Map.fromList . concatMap parseLine . zip [0..] . lines
  where
    parseLine (y,line) = zip (map (flip (,) y) [0..]) (map ((+ (- ord '0')) . ord) line)

data Dir = N | S | E | W deriving (Eq,Ord,Show)

move :: Dir -> (Int,Int) -> (Int,Int)
move N (x,y) = (x,y-1)
move S (x,y) = (x,y+1)
move E (x,y) = (x+1,y)
move W (x,y) = (x-1,y)

turnL :: Dir -> Dir
turnL N = W
turnL W = S
turnL S = E
turnL E = N

turnR :: Dir -> Dir
turnR = turnL . turnL . turnL

-- This is slow - around 11 seconds for my input
search :: Map (Int,Int) Int -> Int
search grid = astar 0 (singleton (h initialState)) empty
  where
    xmax = maximum $ map (fst . fst) $ toList grid
    ymax = maximum $ map (snd . fst) $ toList grid

    initialState :: ((Int,Int),(Dir,Int),Int)
    initialState = ((0,0),(E,0),0) -- ((x,y),(dir,dircount),heatloss)

    h :: ((Int,Int),(Dir,Int),Int) -> (Int,((Int,Int),(Dir,Int),Int))
    h state@((x,y),_,loss) = (loss + xmax-x + ymax-y,state)

    astar :: Int -> Set (Int,((Int,Int),(Dir,Int),Int)) -> Map ((Int,Int),Dir,Int) Int -> Int
    astar maxLoss open visited
--    | loss > maxLoss && traceShow (size open,xy,loss) False = undefined
      | xy == (xmax,ymax) = loss
      | otherwise = astar (max maxLoss loss) (foldr (maybe id insert) newOpen [go dir (count+1), go (turnL dir) 1, go (turnR dir) 1]) (Data.Map.insert (xy,dir,count) loss visited)
      where
        Just ((_,(xy,(dir,count),loss)),newOpen) = minView open
        go newDir newCount
          | newCount > 3 = Nothing
          | not (member newXY grid) = Nothing
          | member (newXY,newDir,newCount) visited && visited!(newXY,newDir,newCount) <= newLoss = Nothing
          | newCount > 0 && member (newXY,newDir,newCount-1) visited && visited!(newXY,newDir,newCount-1) <= newLoss = Nothing
          | newCount > 1 && member (newXY,newDir,newCount-2) visited && visited!(newXY,newDir,newCount-2) <= newLoss = Nothing
          | newCount > 2 && member (newXY,newDir,newCount-3) visited && visited!(newXY,newDir,newCount-3) <= newLoss = Nothing
          | otherwise = Just $ h (newXY,(newDir,newCount),loss + grid!newXY)
          where
            newXY = move newDir xy
            newLoss = loss + grid!newXY

result :: String -> Int
result = search . parse

testData :: String
testData = unlines [
    "2413432311323",
    "3215453535623",
    "3255245654254",
    "3446585845452",
    "4546657867536",
    "1438598798454",
    "4457876987766",
    "3637877979653",
    "4654967986887",
    "4564679986453",
    "1224686865563",
    "2546548887735",
    "4322674655533"
    ]

test :: ()
test
  | result testData /= 102 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/17.txt"

-- This is very slow - around 63 seconds for my input
search2 :: Map (Int,Int) Int -> Int
search2 grid = astar 0 (Data.Set.fromList (map h initialStates)) empty
  where
    xmax = maximum $ map (fst . fst) $ toList grid
    ymax = maximum $ map (snd . fst) $ toList grid

    initialStates :: [((Int,Int),(Dir,Int),Int)]
    initialStates = [((0,0),(W,0),0),((0,0),(N,0),0)]

    h :: ((Int,Int),(Dir,Int),Int) -> (Int,((Int,Int),(Dir,Int),Int))
    h state@((x,y),_,loss) = (loss + xmax-x + ymax-y,state)

    astar :: Int -> Set (Int,((Int,Int),(Dir,Int),Int)) -> Map ((Int,Int),Dir,Int) Int -> Int
    astar maxLoss open visited
--    | loss > maxLoss && traceShow (size open,xy,dir,count,loss) False = undefined
      | xy == (xmax,ymax) = loss
      | otherwise = astar (max maxLoss loss) (foldr (maybe id insert) newOpen [go, turn (turnL dir), turn (turnR dir)]) (Data.Map.insert (xy,dir,count) loss visited)
      where
        Just ((_,(xy,(dir,count),loss)),newOpen) = minView open
        go
          | count >= 10 = Nothing
          | not (member (move dir xy) grid) = Nothing
          | or [member (newXY,dir,count-n) visited && visited!(newXY,dir,count-n) <= newLoss | n <- [0..count-4]] = Nothing
          | otherwise = Just $ h (newXY,(dir,count+1),newLoss)
          where
            newXY = move dir xy
            newLoss = loss + grid!newXY
        turn newDir
          | not (member newXY grid) = Nothing
          | member (newXY,newDir,4) visited && visited!(newXY,newDir,4) <= newLoss = Nothing
          | otherwise = Just $ h (newXY,(newDir,4),newLoss)
          where
            newXY = head $ drop 4 $ iterate (move newDir) xy
            newLoss = loss + sum [grid!loc | loc <- tail $ take 5 $ iterate (move newDir) xy]

result2 :: String -> Int
result2 = search2 . parse

testData2 :: String
testData2 = unlines [
    "111111111111",
    "999999999991",
    "999999999991",
    "999999999991",
    "999999999991"
    ]

test2 :: ()
test2
  | result2 testData /= 94 = error "a"
  | result2 testData2 /= 71 = error "a"
  | otherwise = ()


part2 :: IO Int
part2 = fmap result2 $ readFile "input/17.txt"
