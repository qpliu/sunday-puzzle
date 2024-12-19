module AOC where

import Control.Monad.State(State,evalState,get,modify)
import Data.Array(Array,array)
import Data.Char(isDigit)
import Data.Map(Map,keys)
import qualified Data.Map
import Data.Set(Set,elems)
import qualified Data.Set
import Data.Time(diffUTCTime,getCurrentTime,NominalDiffTime)

data AOC parsed result parsed2 result2 = AOC {
    day :: String,
    testData :: String,
    testResult :: String,
    testData2 :: String,
    testResult2 :: String,
    aocParse :: String -> parsed,
    aocTest :: parsed -> result,
    aocResult :: parsed -> result,
    aocParse2 :: String -> parsed2,
    aocTest2 :: parsed2 -> result2,
    aocResult2 :: parsed2 -> result2
    }

test :: Show result => AOC parsed result parsed2 result2 -> ()
test AOC { testData=td, testResult=tr, aocParse=p, aocTest=r }
  | tr /= (show . r . p) td = error (tr ++ " /= " ++ (show . r . p) td)
  | otherwise = ()

test2 :: Show result2 => AOC parsed result parsed2 result2 -> ()
test2 AOC { testData=td1, testData2=td2, testResult2=tr, aocParse2=p, aocTest2=r }
  | tr /= (show . r . p) td = error (tr ++ " /= " ++ (show . r . p) td)
  | otherwise = ()
  where td | null td2 = td1 | otherwise = td2

part1 :: Show result => AOC parsed result parsed2 result2 -> IO NominalDiffTime
part1 AOC { day=d, aocParse=p, aocResult=r } = do
    t0 <- getCurrentTime
    readFile ("input/" ++ d ++ ".txt") >>= print . r . p
    t1 <- getCurrentTime
    return $ diffUTCTime t1 t0

part2 :: Show result2 => AOC parsed result parsed2 result2 -> IO NominalDiffTime
part2 AOC { day=d, aocParse2=p, aocResult2=r } = do
    t0 <- getCurrentTime
    readFile ("input/" ++ d ++ ".txt") >>= print . r . p
    t1 <- getCurrentTime
    return $ diffUTCTime t1 t0

run :: (Show result, Show result2) => AOC parsed result parsed2 result2 -> IO NominalDiffTime
run aoc = do
    putStr ("Day " ++ day aoc ++ " test part 1: ")
    print $ test aoc
    putStr ("Day " ++ day aoc ++ " part 1: ")
    dt1 <- part1 aoc
    putStrLn ("Day " ++ day aoc ++ " part 1 time: " ++ show dt1)
    putStr ("Day " ++ day aoc ++ " test part 2: ")
    print $ test2 aoc
    putStr ("Day " ++ day aoc ++ " part 2: ")
    dt2 <- part2 aoc
    putStrLn ("Day " ++ day aoc ++ " part 2 time: " ++ show dt2)
    putStrLn ("Day " ++ day aoc ++ " total time: " ++ show (dt1+dt2))
    return $ dt1+dt2

getInput :: AOC parsed result parsed2 result2 -> IO parsed
getInput AOC { day=d, aocParse=p } =
    fmap p $ readFile ("input/" ++ d ++ ".txt")

getInput2 :: AOC parsed result parsed2 result2 -> IO parsed2
getInput2 AOC { day=d, aocParse2=p } =
    fmap p $ readFile ("input/" ++ d ++ ".txt")

parse2d :: String -> Map (Int,Int) Char
parse2d = Data.Map.fromList . p 0 0
  where
    p _ _ [] = []
    p x y (c:cs)
      | c == '\n' = p 0 (y+1) cs
      | otherwise = ((x,y),c) : p (x+1) y cs

parse2da :: String -> Array (Int,Int) Char
parse2da = p 0 0 0 0 []
  where
    p _ _ xmax ymax e [] = array ((0,0),(xmax,ymax)) e
    p x y xmax ymax e (c:cs)
      | c == '\n' = p 0 (y+1) xmax ymax e cs
      | otherwise = p (x+1) y (max xmax x) (max ymax y) (((x,y),c):e) cs

show2dm :: Map (Int,Int) Char -> String
show2dm m
  | xmax-xmin > 150 || ymax-ymin > 150 = gridSize
  | otherwise = unlines $ gridSize :
      [[maybe '.' id $ Data.Map.lookup (x,y) m | x <- [xmin..xmax]]
       | y <- [ymin..ymax]]
  where
    xmax = maximum $ map fst $ keys m
    xmin = minimum $ map fst $ keys m
    ymax = maximum $ map snd $ keys m
    ymin = minimum $ map snd $ keys m
    gridSize = show (xmax-xmin) ++ "×" ++ show (ymax-ymin)

p2dm :: Map (Int,Int) Char -> IO ()
p2dm = putStr . show2dm

show2ds :: Set (Int,Int) -> String
show2ds = show2dm . Data.Map.fromList . flip zip (repeat '#') . elems

p2ds :: Set (Int,Int) -> IO ()
p2ds = putStr . show2ds

parseInts :: String -> [Int]
parseInts = p . dropWhile notStart
  where
    notStart c = not (isDigit c || c == '-')
    p "" = []
    p "-" = []
    p ('-':str@(c:_))
      | not (isDigit c) = parseInts str
      | otherwise = read ('-':n) : parseInts rest
      where (n,rest) = span isDigit str
    p str = read n : parseInts rest
      where (n,rest) = span isDigit str

type Memo a b = State (Map a b)

evalMemoized :: Memo a b c -> c
evalMemoized expr = evalState expr Data.Map.empty

memoize :: Ord a => (a -> Memo a b b) -> a -> Memo a b b
memoize f a = do
    memo <- get
    maybe eval return $ Data.Map.lookup a memo
  where
    eval = do
        b <- f a
        modify (Data.Map.insert a b)
        return b

astarWithVisited :: (Ord cost, Ord path, Ord state) =>
    (path -> Map state cost -> result)
    -> (path -> cost) -> (path -> [path]) -> (path -> state)
    -> (path -> Bool) -> [path] -> Maybe result
astarWithVisited makeResult heuristic neighbors toState done initialPaths =
    search (Data.Set.fromList [(heuristic p,p) | p <- initialPaths], 
            Data.Map.empty)
  where
   search (open,visited)
      | Data.Set.null open = Nothing
      | done curPath = Just $ makeResult curPath visited
      | otherwise =
          search $ foldr check (poppedOpen,visited) $ neighbors curPath
      where
        Just ((_,curPath),poppedOpen) = Data.Set.minView open
        check nextPath (open,visited)
          | maybe True (cost <) $ Data.Map.lookup (toState nextPath) visited =
              (Data.Set.insert (cost,nextPath) open,
               Data.Map.insert (toState nextPath) cost visited)
          | otherwise = (open,visited)
          where cost = heuristic nextPath

astar :: (Ord cost, Ord path, Ord state) =>
    (path -> cost) -> (path -> [path]) -> (path -> state)
    -> (path -> Bool) -> [path] -> Maybe path
astar = astarWithVisited const

astarAll :: (Ord cost, Ord path, Ord state) =>
    (path -> cost) -> (path -> [path]) -> (path -> state)
    -> (path -> Bool) -> ([path] -> best) -> (best -> [path] -> best)
    -> [path] -> Maybe best
astarAll heuristic neighbors toState done makeBest mergeBest initialPaths =
    maybe Nothing init firstSearch
  where
    heuristicA (path:_) = heuristic path
    neighborsA (history@(path:_)) = map (:history) (neighbors path)
    toStateA (path:_) = toState path
    doneA (path:_) = done path
    firstSearch = astarWithVisited (,) heuristicA neighborsA toStateA doneA
                                   (map (:[]) initialPaths)
    init (firstBestPath,firstVisited) =
        search (initialBranches,firstVisited,visitedBest,initialBest,bestCost)
      where
        toVisitedBest path = (toState path,heuristic path)
        visitedBest = Data.Map.fromList $ map toVisitedBest firstBestPath
        bestCost = heuristicA firstBestPath

        initialBest = makeBest firstBestPath

        initialBranches =
            Data.Set.fromList $ map toOpen
                              $ filter viableBranch
                              $ concatMap neighbors firstBestPath
          where toOpen path = (heuristic path,[path])
        viableBranch path
          | Data.Map.member (toState path) visitedBest = False
          | otherwise = maybe True (heuristic path ==)
                              $ Data.Map.lookup (toState path) firstVisited

    search (open,visited,visitedBest,best,bestCost)
      | Data.Set.null open = Just best
      | curDone && curCost == bestCost = search mergeCurrent
      | curDone = search visitCurrent
      | maybe False (curCost ==)
              (Data.Map.lookup (toStateA curPath) visitedBest) =
          search mergeCurrent
      | otherwise = search enqueueNeighbors
      where
        Just ((curCost,curPath),poppedOpen) = Data.Set.minView open
        curDone = doneA curPath

        mergeCurrent = (poppedOpen,
                        foldr visit visited curPath,
                        foldr visit visitedBest curPath,
                        mergeBest best curPath,
                        bestCost)
        visitCurrent = (poppedOpen,
                        foldr visit visited curPath,
                        visitedBest,best,bestCost)
        enqueueNeighbors = foldr enqueue
                                 (poppedOpen,visited,visitedBest,best,bestCost)
                               $ neighborsA curPath

        visit path dict =
            Data.Map.alter
                (Just . maybe (heuristic path) (min (heuristic path)))
                (toState path) dict
        enqueue path (open,visited,visitedBest,best,bestCost)
          | maybe True (cost <=) $ Data.Map.lookup (toStateA path) visited =
              (Data.Set.insert (cost,path) open,
               Data.Map.insert (toStateA path) cost visited,
               visitedBest,best,bestCost)
          | otherwise = (open,visited,visitedBest,best,bestCost)
          where cost = heuristicA path
