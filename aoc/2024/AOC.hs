module AOC where

import Data.Map(Map,fromList)
import Data.Time(diffUTCTime,getCurrentTime)

data AOC parsed result parsed2 result2 = AOC {
    day :: String,
    testData :: String,
    testResult :: String,
    testData2 :: String,
    testResult2 :: String,
    aocParse :: String -> parsed,
    aocResult :: parsed -> result,
    aocParse2 :: String -> parsed2,
    aocResult2 :: parsed2 -> result2
    }

test :: Show result => AOC parsed result parsed2 result2 -> ()
test AOC { testData=td, testResult=tr, aocParse=p, aocResult=r }
  | tr /= (show . r . p) td = error (tr ++ " /= " ++ (show . r . p) td)
  | otherwise = ()

test2 :: Show result2 => AOC parsed result parsed2 result2 -> ()
test2 AOC { testData=td1, testData2=td2, testResult2=tr, aocParse2=p, aocResult2=r }
  | tr /= (show . r . p) td = error (tr ++ " /= " ++ (show . r . p) td)
  | otherwise = ()
  where td | null td2 = td1 | otherwise = td2

part1 :: Show result => AOC parsed result parsed2 result2 -> IO ()
part1 AOC { day=d, aocParse=p, aocResult=r } = do
    t0 <- getCurrentTime
    res <- fmap (r . p) $ readFile ("input/" ++ d ++ ".txt")
    print res
    t1 <- getCurrentTime
    print $ diffUTCTime t1 t0

part2 :: Show result2 => AOC parsed result parsed2 result2 -> IO ()
part2 AOC { day=d, aocParse2=p, aocResult2=r } = do
    t0 <- getCurrentTime
    res <- fmap (r . p) $ readFile ("input/" ++ d ++ ".txt")
    print res
    t1 <- getCurrentTime
    print $ diffUTCTime t1 t0

run :: (Show result, Show result2) => AOC parsed result parsed2 result2 -> IO ()
run aoc = do
    putStr "Test part 1: "
    print $ test aoc
    putStr "Part 1: "
    part1 aoc
    putStr "Test part 2: "
    print $ test2 aoc
    putStr "Part 2: "
    part2 aoc

parse2d :: String -> Map (Int,Int) Char
parse2d = fromList . p 0 0
  where
    p _ _ [] = []
    p x y (c:cs)
      | c == '\n' = p 0 (y+1) cs
      | otherwise = ((x,y),c) : p (x+1) y cs
