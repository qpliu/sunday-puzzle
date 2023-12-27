import Data.Time(diffUTCTime,getCurrentTime)

-----------------------------------------------------------------------------

parse :: Params -> String -> Parsed
parse = undefined

result :: Parsed -> Result
result = undefined

parse2 :: Params -> String -> Parsed
parse2 = parse

result2 :: Parsed -> Result
result2 = undefined

-----------------------------------------------------------------------------
type Params = ()
type Parsed = [Int]
type Result = Int
type Result2 = Int

inputFile :: String
inputFile = "input/01.txt"

testResult :: Result
testResult = 1234

testData :: String
testData = unlines [

    ]

testResult2 :: Result2
testResult2 = 1234

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
