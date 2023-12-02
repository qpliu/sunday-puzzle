import Data.Char(isDigit,ord)

parse :: String -> Int
parse text
  | null t = 0
  | otherwise = 10*ord (head t) + ord (last t) - 11*ord '0'
  where t = filter isDigit text    

testData :: String
testData = unlines ["1abc2","pqr3stu8vwx","a1b2c3d4e5f","treb7uchet"]

result :: String -> Int
result = sum . map parse . lines

test :: ()
test
  | result testData /= 142 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/01.txt"

testData2 :: String
testData2 = unlines [
    "two1nine","eightwothree","abcone2threexyz","xtwone3four",
    "4nineeightseven2","zoneight234","7pqrstsixteen"
    ]

-- tricky: numbers can overlap
parse2 :: String -> [Int]
parse2 ('0':rest) = 0 : parse2 rest
parse2 ('z':rest@('e':'r':'o':_)) = 0 : parse2 rest
parse2 ('1':rest) = 1 : parse2 rest
parse2 ('o':rest@('n':'e':_)) = 1 : parse2 rest
parse2 ('2':rest) = 2 : parse2 rest
parse2 ('t':rest@('w':'o':_)) = 2 : parse2 rest
parse2 ('3':rest) = 3 : parse2 rest
parse2 ('t':rest@('h':'r':'e':'e':_)) = 3 : parse2 rest
parse2 ('4':rest) = 4 : parse2 rest
parse2 ('f':rest@('o':'u':'r':_)) = 4 : parse2 rest
parse2 ('5':rest) = 5 : parse2 rest
parse2 ('f':rest@('i':'v':'e':_)) = 5 : parse2 rest
parse2 ('6':rest) = 6 : parse2 rest
parse2 ('s':rest@('i':'x':_)) = 6 : parse2 rest
parse2 ('7':rest) = 7 : parse2 rest
parse2 ('s':rest@('e':'v':'e':'n':_)) = 7 : parse2 rest
parse2 ('8':rest) = 8 : parse2 rest
parse2 ('e':rest@('i':'g':'h':'t':_)) = 8 : parse2 rest
parse2 ('9':rest) = 9 : parse2 rest
parse2 ('n':rest@('i':'n':'e':_)) = 9 : parse2 rest
parse2 (_:rest) = parse2 rest
parse2 [] = []

build2 :: [Int] -> Int
build2 digits | null digits = 0 | otherwise = 10*head digits + last digits

result2 :: String -> Int
result2 = sum . map (build2 . parse2) . lines

test2 :: ()
test2
  | result2 testData2 /= 281 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/01.txt"
