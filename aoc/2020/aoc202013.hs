import Data.Char(isDigit)
import Data.Ratio(denominator)

parse1 :: String -> (Int,[Int])
parse1 str = (head list,tail list)
  where
    list = (map read . words . map toSpace) str
    toSpace c | c `elem` ",x" = ' ' | otherwise = c

addWait :: Int -> Int -> (Int,Int)
addWait t0 busID = ((-t0) `mod` busID,busID)

addWaits :: (Int,[Int]) -> [(Int,Int)]
addWaits (t0,busIDs) = map (addWait t0) busIDs

testData :: String
testData = "939\n7,13,x,x,59,x,31,19\n"

test :: ()
test
  | (uncurry (*) . minimum . addWaits . parse1) testData /= 295 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (uncurry (*) . minimum . addWaits . parse1) $ readFile "input/13.txt"

parse2 :: String -> [(Int,Int)]
parse2 = p 0 . dropWhile isDigit
  where
    p i "" = []
    p i "\n" = []
    p i (_:'x':rest) = p (i+1) rest
    p i str = (i,read n) : p (i+1) rest
      where (n,rest) = span isDigit (drop 1 str)

-- Given a,x,b,x,x,x,c
-- look for n*a + 2 = m*b, (n*a + 2) mod b = 0
-- then look for n*a + 5 = p*c, (n*a + 5) mod c = 0

-- In general, look for n*busid[0] + delay[i] = m[i]*busid[i]
-- where n and m[i] are integers.
-- (n*busid[0] + delay[i]) mod busid[i] = 0

-- This is very slow, but gives the right results for the test cases.
findT :: [(Int,Int)] -> [Int]
findT ((0,busid0):buses) = map (busid0*) $ foldr sieve [1..] buses
  where
    sieve (i,busid) ns = filter (f i busid) ns
    f i busid n = (n*busid0 + i) `mod` busid == 0

-- Internet hints point to
-- https://en.wikipedia.org/wiki/Chinese_remainder_theorem
-- I note that all the busids are prime, making dividing out the gcd moot.
findT2 :: [(Int,Int)] -> Int
findT2 buses = s - nn*(s `div` nn)
  where
    s = sum $ map f buses
    nn = foldr prod 1 (map snd buses)
    prod a b = a*b `div` gcd a b
    f (delay,busid) = (nn `div` busid)*(g (nn `div` busid) busid ((-delay) `mod` busid))
    g a b c = search 0
      -- find x such that mod (a*x) b = c
      -- minimum x means x < b
      where search x | mod (a*x) b == c = x | otherwise = search (x+1)

testData2 :: [String]
testData2 = ["939\n7,13,x,x,59,x,31,19\n","3417 17,x,13,19","754018 67,7,59,61","779210 67,x,7,59,61","1261476 67,7,x,59,61","1202161486 1789,37,47,1889"]

test2 :: ()
test2
  | (findT2 . parse2) (testData2 !! 0) /= 1068781 = error "a"
  | (findT2 . parse2) (testData2 !! 1) /= 3417 = error "b"
  | (findT2 . parse2) (testData2 !! 2) /= 754018 = error "c"
  | (findT2 . parse2) (testData2 !! 3) /= 779210 = error "d"
  | (findT2 . parse2) (testData2 !! 4) /= 1261476 = error "e"
  | (findT2 . parse2) (testData2 !! 5) /= 1202161486 = error "f"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (findT2 . parse2) $ readFile "input/13.txt"
