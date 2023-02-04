import Data.Char(isDigit)

data SNum = SNum Int | Pair SNum SNum deriving (Eq,Show)

parse :: String -> SNum
parse = fst . parse1
  where
    parse1 ('[':rest) = (Pair n1 n2,rest2)
      where
        (n1,',':rest1) = parse1 rest
        (n2,']':rest2) = parse1 rest1
    parse1 str = (SNum (read n),rest)
      where (n,rest) = span isDigit str

display :: SNum -> String
display (SNum n) = show n
display (Pair n1 n2) = "["++display n1++","++display n2++"]"

reduce :: SNum -> SNum
reduce snum = maybe snum reduce $ maybe (split snum) (Just . fst) (explode 0 snum)

split :: SNum -> Maybe SNum
split (SNum n)
  | n < 10 = Nothing
  | otherwise = Just (Pair (SNum (n `div` 2)) (SNum ((n+1) `div` 2)))
split (Pair n1 n2) = maybe (maybe Nothing (Just . Pair n1) (split n2))
                           (Just . flip Pair n2)
                           (split n1)
                           
explode :: Int -> SNum -> Maybe (SNum,(Int,Int))
explode depth snum@(Pair n1 n2)
  | depth >= 4 = Just (SNum 0,(magnitude n1,magnitude n2))
  | otherwise = maybe explodeN2 Just explodeN1
  where
    explodeN1 = fmap (\ (new1,(l,r)) -> (Pair new1 (explodeRight n2 r),(l,0))) $ explode (depth+1) n1
    explodeN2 = fmap (\ (new2,(l,r)) -> (Pair (explodeLeft n1 l) new2,(0,r))) $ explode (depth+1) n2
explode _ _ = Nothing

explodeLeft :: SNum -> Int -> SNum
explodeLeft (SNum n) x = SNum (n+x)
explodeLeft (Pair n1 n2) x = Pair n1 (explodeLeft n2 x)

explodeRight :: SNum -> Int -> SNum
explodeRight (SNum n) x = SNum (n+x)
explodeRight (Pair n1 n2) x = Pair (explodeRight n1 x) n2

add :: SNum -> SNum -> SNum
add x y = reduce $ Pair x y

ssum :: [SNum] -> SNum
ssum (n1:ns) = foldl add n1 ns

magnitude :: SNum -> Int
magnitude (SNum x) = x
magnitude (Pair x y) = 3*magnitude x+2*magnitude y

testData :: [(String,String)]
testData = [
    ("[1,1]\n[2,2]\n[3,3]\n[4,4]\n","[[[[1,1],[2,2]],[3,3]],[4,4]]"),
    ("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]\n[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]\n[7,[5,[[3,8],[1,4]]]]\n[[2,[2,2]],[8,[8,1]]]\n[2,9]\n[1,[[[9,3],9],[[9,0],[0,7]]]]\n[[[5,[7,4]],7],1]\n[[[[4,2],2],6],[8,7]]\n","[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"),
    ("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]\n","[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")
    ]

failTest :: (String,String) -> Bool
failTest (input,expectedResult) = ssum (map parse (lines input)) /= parse expectedResult

test :: ()
test
  | any failTest testData = error "a"
  | (magnitude . parse . snd) (testData !! 1) /= 3488 = error "b"
  | (magnitude . parse . snd) (testData !! 2) /= 4140 = error "c"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (magnitude . ssum . map parse . lines) $ readFile "input/18.txt"

sums :: [SNum] -> [SNum]
sums nums = [add x y | x <- nums, y <- nums]

test2 :: ()
test2
  | (maximum . map magnitude . sums . map parse . lines . fst) (testData !! 2) /= 3993 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (maximum . map magnitude . sums . map parse . lines) $ readFile "input/18.txt"
