module AOC202421 where

import AOC

aoc = AOC {
    day="21",
    testData=unlines [
    "029A",
    "980A",
    "179A",
    "456A",
    "379A"
    ],
    testResult="126384",
    testData2="",
    testResult2="126384",
    aocParse=words,
    aocTest=result 2,
    aocResult=result 2,
    aocParse2=words,
    aocTest2=result 2,
    aocResult2=result 25
    }

npadMoves :: (Char,Char) -> [String]
npadMoves (from,to)
  | x0 == 0 && y1 == 3 =
      [replicate x1 '>' ++ replicate (y1-y0) 'v' ++ "A"]
  | y0 == 3 && x1 == 0 =
      [replicate (3-y1) '^' ++ replicate x0 '<' ++ "A"]
  | otherwise =
      [replicate (x1-x0) '>' ++ replicate (x0-x1) '<'
           ++ replicate (y0-y1) '^' ++ replicate (y1-y0) 'v' ++ "A",
       replicate (y0-y1) '^' ++ replicate (y1-y0) 'v'
           ++ replicate (x1-x0) '>' ++ replicate (x0-x1) '<' ++ "A"]
  where
    (x0,y0) = xy from
    (x1,y1) = xy to
    xy '0' = (1,3)
    xy 'A' = (2,3)
    xy '1' = (0,2)
    xy '2' = (1,2)
    xy '3' = (2,2)
    xy '4' = (0,1)
    xy '5' = (1,1)
    xy '6' = (2,1)
    xy '7' = (0,0)
    xy '8' = (1,0)
    xy '9' = (2,0)

dpadMoves :: (Char,Char) -> [String]
dpadMoves (from,to)
  | x0 == 0 =
      [replicate x1 '>' ++ replicate (y0-y1) '^' ++ "A"]
  | y0 == 0 && x1 == 0 =
      [replicate y1 'v'
           ++ replicate (x1-x0) '>' ++ replicate (x0-x1) '<' ++ "A"]
  | otherwise =
      [replicate (x1-x0) '>' ++ replicate (x0-x1) '<'
           ++ replicate (y0-y1) '^' ++ replicate (y1-y0) 'v' ++ "A",
       replicate (y0-y1) '^' ++ replicate (y1-y0) 'v'
           ++ replicate (x1-x0) '>' ++ replicate (x0-x1) '<' ++ "A"]
  where
    (x0,y0) = xy from
    (x1,y1) = xy to
    xy '^' = (1,0)
    xy 'A' = (2,0)
    xy '<' = (0,1)
    xy 'v' = (1,1)
    xy '>' = (2,1)

robotMove = memoize rm
  where
    rm (nrobots,from,to) = mapM (robotPress (nrobots-1)) $ dpadMoves (from,to)

robotPress nrobots code
  | nrobots == 0 = return $ length code
  | otherwise = rp 0 ('A':code)
  where
    rp count [_] = return count
    rp count (from:rest@(to:_)) = do
        counts <- robotMove (nrobots,from,to)
        rp (count+minimum counts) rest

pressCount nrobots code = p 0 ('A':code)
  where
    p count [_] = return count
    p count (from:rest@(to:_)) = do
        counts <- mapM (robotPress nrobots) $ npadMoves (from,to)
        p (count+minimum counts) rest

complexity nrobots code = do
    n <- pressCount nrobots code
    return $ read (init (dropWhile (== '0') code )) * n

-- 176650 is the right part 1 answer for my input
-- 217698355426872 is the right part 2 answer for my input
result nrobots codes = sum $ evalMemoized (mapM (complexity nrobots) codes)
