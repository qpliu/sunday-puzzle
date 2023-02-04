import Data.Map(Map,alter,empty,findWithDefault)

readForCheck :: Int -> Int -> String -> [Map Char Int]
readForCheck w h input
  | null input = []
  | otherwise = tabulate (take (w*h) input) : readForCheck w h (drop (w*h) input)
  where
    tabulate = foldr (alter (Just . maybe 1 (+1))) empty

check :: [Map Char Int] -> Int
check = snd . minimum . map counts
  where
    counts table = (count '0',count '1' * count '2')
      where count ch = findWithDefault 0 ch table

part1 :: IO Int
part1 = fmap (check . readForCheck 25 6 . filter (/= '\n')) $ readFile "input/08.txt"

parse :: Int -> Int -> String -> [String]
parse w h input
  | null input = []
  | otherwise = layer : parse w h rest
  where (layer,rest) = splitAt (w*h) input

merge :: String -> String -> String
merge top bottom = zipWith mergePixel top bottom
  where
    mergePixel '2' b = b
    mergePixel t _ = t

display :: Int -> String -> [String]
display w str
  | null str = []
  | otherwise = line : display w rest where (line,rest) = splitAt w str

part2 :: IO ()
part2 = do
    input <- readFile "input/08.txt"
    putStr $ unlines $ display 25 $ foldl merge (repeat '2') $ parse 25 6 $ filter (/= '\n') input
