-- This will probably be too slow, but for 10007 cards, it's not too slow.
import Data.List(sort)

shuffle :: Int -> ([Int],[String]) -> ([Int],[String])
shuffle len (deck,("deal":"into":"new":"stack":rest)) = (reverse deck,rest)
shuffle len (deck,("cut":nstring:rest))
  | n >= 0 = (b++a,rest)
  | otherwise = (z++y,rest)
  where
    n = read nstring
    (a,b) = splitAt n deck
    (y,z) = splitAt (len+n) deck
shuffle len (deck,("deal":"with":"increment":nstring:rest)) =
    (map snd $ sort $ zip newpos deck,rest)
  where
    n = read nstring
    newpos = [(i*n) `mod` len | i <- [0..]]
shuffle len (deck,_) = (deck,[])

testData :: [([Int],String)]
testData = [
    ([0, 3, 6, 9, 2, 5, 8, 1, 4, 7],"deal with increment 7\ndeal into new stack\ndeal into new stack"),
    ([3, 0, 7, 4, 1, 8, 5, 2, 9, 6],"cut 6\ndeal with increment 7\ndeal into new stack"),
    ([6, 3, 0, 7, 4, 1, 8, 5, 2, 9],"deal with increment 7\ndeal with increment 9\ncut -2"),
    ([9, 2, 5, 8, 1, 4, 7, 0, 3, 6],"deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\ndeal with increment 9\ndeal with increment 3\ncut -1")
    ]

run :: Int -> String -> [Int]
run n input = fst $ head $ dropWhile (not . null . snd) $ iterate (shuffle n) ([0..n-1],words input)

test :: ()
test
  | any (uncurry (/=) . fmap (run 10) ) testData = error "a"
  | otherwise = ()

-- It's faster to just follow one card.
parse1 :: Int -> [String] -> Int -> Int
parse1 len ("deal":"into":"new":"stack":rest) = parse1 len rest . ((len - 1) -)
parse1 len ("cut":nstring:rest) = parse1 len rest . cut
  where
    n = read nstring
    cut i = (i - n) `mod` len
parse1 len ("deal":"with":"increment":nstring:rest) = parse1 len rest . deal
  where
    n = read nstring
    deal i = (i*n) `mod` len
parse1 _ _ = id

run1 :: Int -> Int -> String -> Int
run1 card len input = parse1 len (words input) card

part1 :: IO Int
part1 = fmap (run1 2019 10007) $ readFile "input/22.txt"

-- For part 2, invert the process to find where 2020th comes from, then
-- find where that comes from, etc, and find the periodicity.
parse2 :: Int -> [String] -> Int -> Int
parse2 len ("deal":"into":"new":"stack":rest) = ((len - 1) -) . parse2 len rest
parse2 len ("cut":nstring:rest) = uncut . parse2 len rest
  where
    n = read nstring
    uncut i = (i + n) `mod` len
parse2 len ("deal":"with":"increment":nstring:rest) = undeal . parse2 len rest
  where
    n = read nstring
    undeal i
      | i `mod` n /= 0 = undeal (i+len)
      | otherwise = i `div` n
parse2 _ _ = id

test2 :: IO ()
test2 = do
    input <- fmap words $ readFile "input/22.txt"
    let f = parse1 10007 input
    let finv = parse2 10007 input
    if (finv . f) 2019 /= 2019 then error "a" else return ()

-- The period is too long.  Have to resort to internet hints.

parse3 :: Integer -> [String] -> (Integer,Integer) -> (Integer,Integer)
parse3 len ("deal":"into":"new":"stack":rest) (a,b) = parse3 len rest ((-a) `mod` len, (-b-1) `mod` len)
parse3 len ("cut":nstring:rest) (a,b) = parse3 len rest (a, (b-n) `mod` len)
  where n = read nstring
parse3 len ("deal":"with":"increment":nstring:rest) (a,b) = parse3 len rest ((a*n)`mod`len, (b*n)`mod`len)
  where n = read nstring
parse3 _ _ ab = ab

apply3 :: Integer -> (Integer,Integer) -> Integer -> Integer
apply3 len (a,b) card = (a*card + b) `mod` len

powMod :: Integer -> Integer -> Integer -> Integer
powMod x y m = f (x `rem` m) y 1 `mod` m
  where
    f _ 0 acc = acc
    f b e acc = f (b * b `rem` m) (e `quot` 2)
      (if odd e then b * acc `rem` m else acc)

-- Taken from internet hints.  I don't know how this works.
invert3n :: Integer -> Integer -> (Integer,Integer) -> Integer -> Integer
invert3n len n (a,b) pos = ((pos - r)*powMod a (n*(len-2)) len + r) `mod` len
  where r = (b*powMod (1-a) (len-2) len) `mod` len

part2 :: IO ()
part2 = do
    input <- readFile "input/22.txt"
    let ab1 = parse3 10007 (words input) (1,0)
    print ("Part 1",apply3 10007 ab1 2019)
    let ab2 = parse3 119315717514047 (words input) (1,0)
    print ("Part 2",invert3n 119315717514047 101741582076661 ab2 2020)
