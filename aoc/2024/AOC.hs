module AOC where

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

astar :: (Ord cost, Ord path, Ord state) =>
    (path -> cost) -> (path -> [path]) -> (path -> state)
    -> (path -> Bool) -> [path] -> path
astar heuristic neighbors toState done initialPaths =
    search (Data.Set.fromList [(heuristic p,p) | p <- initialPaths], 
            Data.Map.empty)
  where
    search (open,visited)
      | done curPath = curPath
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

astarAll :: (Ord cost, Ord path, Ord state) =>
    (path -> cost) -> (path -> [path]) -> (path -> state)
    -> (path -> Bool) -> [path] -> [path]
astarAll heuristic neighbors toState done initialPaths =
    search Nothing
           (Data.Set.fromList [(heuristic p,p) | p <- initialPaths], 
            Data.Map.empty)
  where
    search best (open,visited)
      | Data.Set.null open = []
      | curDone && maybe False (curCost >) best = []
      | curDone = curPath : search (Just curCost) (poppedOpen,visited)
      | otherwise =
          search best $ foldr check (poppedOpen,visited) $ neighbors curPath
      where
        Just ((curCost,curPath),poppedOpen) = Data.Set.minView open
        curDone = done curPath
        check nextPath (open,visited)
          | maybe True (cost <=) $ Data.Map.lookup (toState nextPath) visited =
              (Data.Set.insert (cost,nextPath) open,
               Data.Map.insert (toState nextPath) cost visited)
          | otherwise = (open,visited)
          where cost = heuristic nextPath
