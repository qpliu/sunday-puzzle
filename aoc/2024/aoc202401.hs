import Data.Map(alter,empty)
import qualified Data.Map
import Data.List(sort)
import Data.Time(diffUTCTime,getCurrentTime)

-----------------------------------------------------------------------------

parse :: Params -> String -> Parsed
parse _ = parseWords ([],[]) . words
  where
    parseWords :: ([Int],[Int]) -> [String] -> ([Int],[Int])
    parseWords (l,r) (a:b:rest) = parseWords (read a:l,read b:r) rest
    parseWords lr [] = lr

result :: Parsed -> Result
result (l,r) = sum $ map abs $ zipWith (-) (sort l) (sort r)

parse2 :: Params -> String -> Parsed
parse2 = parse

result2 :: Parsed -> Result2
result2 (l,r) = sum $ map score l
  where
    counts = foldr (alter (Just . maybe 1 (+1))) empty r
    count i = maybe 0 id $ Data.Map.lookup i counts
    score i = i*count i

-----------------------------------------------------------------------------
type Params = ()
type Parsed = ([Int],[Int])
type Result = Int
type Result2 = Int

inputFile :: String
inputFile = "input/01.txt"

testResult :: Result
testResult = 11

testData :: String
testData = unlines [
    "3   4",
    "4   3",
    "2   5",
    "1   3",
    "3   9",
    "3   3"
    ]

testResult2 :: Result2
testResult2 = 31

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
