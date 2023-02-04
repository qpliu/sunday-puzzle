import Data.Char(chr,ord,isDigit)

ord0 :: Int
ord0 = ord '0'

parse :: String -> [Int]
parse = map ((+ (-ord0)) . ord) . filter isDigit

display :: [Int] -> String
display = map (chr . (+ord0))

pattern :: Int -> Int -> Int
pattern row col | odd p = 2 - p | otherwise = 0
  where p = (col `div` row) `mod` 4

fftPhase :: Int -> [Int] -> [Int]
fftPhase len signal = map applyPattern [1..len]
  where
    applyPattern row = (`mod` 10) $ abs $ sum $ zipWith (*) signal $ map (pattern row) [1..len]

fft :: Int -> [Int] -> [Int]
fft nphases signal = head $ drop nphases $ iterate (fftPhase (length signal)) signal

testData :: [(Int,String,String)]
testData = [
    (0,"12345678","12345678"),
    (1,"12345678","48226158"),
    (2,"12345678","34040438"),
    (3,"12345678","03415518"),
    (4,"12345678","01029498"),
    (100,"80871224585914546619083218645595","24176176"),
    (100,"19617804207202209144916044189917","73745418"),
    (100,"69317163492948606335995924319873","52432133")]

test :: ()
test
  | any testItem testData = error "a"
  | otherwise = ()
  where
    testItem (nphases,signal,expected) = expected /= take (length expected) (display $ fft nphases $ parse signal)

part1 :: IO String
part1 = fmap (display . take 8 . fft 100 . parse) $ readFile "input/16.txt"

-- For part 2

-- I think 100^2 = 10000 is a key.  Apparently not.
-- Had to look at hints on the internet.

getOffset :: [Int] -> Int
getOffset input = sum $ zipWith (*) [10^i | i <- [6,5..0]] input

init2 :: String -> [Int]
init2 str
  | len > offset = error "algorithm invalid"
  | otherwise = reverse $ take len $ cycle $ reverse input
  where
    input = parse str
    offset = getOffset input
    len = 10000*length input - offset

phase2 :: [Int] -> [Int]
phase2 signal = snd $ foldr collect (0,[]) signal
  where collect oldDigit (nextNewDigit,result) = (newDigit,newDigit:result)
            where newDigit = (nextNewDigit + oldDigit) `mod` 10

fft2 :: Int -> [Int] -> [Int]
fft2 nphases signal = head $ drop nphases $ iterate phase2 signal

testData2 :: [(String,String)]
testData2 = [
    ("84462026","03036732577212944063491565474664"),
    ("78725270","02935109699940807407585447034323"),
    ("53553731","03081770884921959731165446850517")
    ]

test2 :: ()
test2
  | any (uncurry (/=) . fmap (display . take 8 . fft2 100 . init2)) testData2 = error "a"
  | otherwise = ()

part2 :: IO String
part2 = fmap (display . take 8 . fft2 100 . init2) $ readFile "input/16.txt"
