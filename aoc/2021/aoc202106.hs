type Fish = (Integer,Integer,Integer,Integer,Integer,Integer,Integer,Integer,Integer)

parse :: String -> Fish
parse = p (0,0,0,0,0,0,0,0,0)
  where
    p (f0,f1,f2,f3,f4,f5,f6,f7,f8) ('0':rest) = p (f0+1,f1,f2,f3,f4,f5,f6,f7,f8) rest
    p (f0,f1,f2,f3,f4,f5,f6,f7,f8) ('1':rest) = p (f0,f1+1,f2,f3,f4,f5,f6,f7,f8) rest
    p (f0,f1,f2,f3,f4,f5,f6,f7,f8) ('2':rest) = p (f0,f1,f2+1,f3,f4,f5,f6,f7,f8) rest
    p (f0,f1,f2,f3,f4,f5,f6,f7,f8) ('3':rest) = p (f0,f1,f2,f3+1,f4,f5,f6,f7,f8) rest
    p (f0,f1,f2,f3,f4,f5,f6,f7,f8) ('4':rest) = p (f0,f1,f2,f3,f4+1,f5,f6,f7,f8) rest
    p (f0,f1,f2,f3,f4,f5,f6,f7,f8) ('5':rest) = p (f0,f1,f2,f3,f4,f5+1,f6,f7,f8) rest
    p (f0,f1,f2,f3,f4,f5,f6,f7,f8) ('6':rest) = p (f0,f1,f2,f3,f4,f5,f6+1,f7,f8) rest
    p (f0,f1,f2,f3,f4,f5,f6,f7,f8) ('7':rest) = p (f0,f1,f2,f3,f4,f5,f6,f7+1,f8) rest
    p (f0,f1,f2,f3,f4,f5,f6,f7,f8) ('8':rest) = p (f0,f1,f2,f3,f4,f5,f6,f7,f8+1) rest
    p fish (_:rest) = p fish rest
    p fish [] = fish

day :: Fish -> Fish
day (f0,f1,f2,f3,f4,f5,f6,f7,f8) = (f1,f2,f3,f4,f5,f6,f7+f0,f8,f0)

count :: Fish -> Integer
count (f0,f1,f2,f3,f4,f5,f6,f7,f8) = f0+f1+f2+f3+f4+f5+f6+f7+f8

testData :: String
testData = "3,4,3,1,2"

test :: ()
test
  | (count . head . drop 18 . iterate day . parse) testData /= 26 = error "a"
  | (count . head . drop 80 . iterate day . parse) testData /= 5934 = error "b"
  | otherwise = ()

part1 :: IO Integer
part1 = fmap (count . head . drop 80 . iterate day . parse) $ readFile "input/06.txt"

test2 :: ()
test2
  | (count . head . drop 256 . iterate day . parse) testData /= 26984457539 = error "a"
  | otherwise = ()

part2 :: IO Integer
part2 = fmap (count . head . drop 256 . iterate day . parse) $ readFile "input/06.txt"
