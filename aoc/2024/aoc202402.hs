import Data.Time(diffUTCTime,getCurrentTime)

-----------------------------------------------------------------------------

parse :: Params -> String -> Parsed
parse _ = map (map read . words) . lines

result :: Parsed -> Result
result = length . filter safe

safe :: [Int] -> Bool
safe r = all (`elem` [1,2,3]) diffs || all (`elem` [-1,-2,-3]) diffs
  where diffs = zipWith (-) r (tail r)

parse2 :: Params -> String -> Parsed
parse2 = parse

result2 :: Parsed -> Result
result2 = length . filter dampedSafe

dampedSafe :: [Int] -> Bool
dampedSafe r
  | safe r = True
  | otherwise = any (safe . rm) [0..length r-1]
    where
      rm i = take i r ++ drop (i+1) r

-----------------------------------------------------------------------------
type Params = ()
type Parsed = [[Int]]
type Result = Int
type Result2 = Int

inputFile :: String
inputFile = "input/02.txt"

testResult :: Result
testResult = 2

testData :: String
testData = unlines [
    "7 6 4 2 1",
    "1 2 7 8 9",
    "9 7 6 2 1",
    "1 3 2 4 5",
    "8 6 4 4 1",
    "1 3 6 7 9"
    ]

testResult2 :: Result2
testResult2 = 4

testData2 :: String
testData2 = testData

testParams :: Params
testParams = ()

params :: Params
params = ()
-----------------------------------------------------------------------------
test :: ()
test
  | result (parse testParams testData) /= testResult = error "a"
  | otherwise = ()

part1 :: IO ()
part1 = do
    t0 <- getCurrentTime
    r <- fmap (result . parse params) $ readFile inputFile
    print r
    t1 <- getCurrentTime
    print $ diffUTCTime t1 t0

test2 :: ()
test2
  | result2 (parse2 testParams testData2) /= testResult2 = error "a"
  | otherwise = ()

part2 :: IO ()
part2 = do
    t0 <- getCurrentTime
    r <- fmap (result2 . parse2 params) $ readFile inputFile
    print r
    t1 <- getCurrentTime
    print $ diffUTCTime t1 t0
