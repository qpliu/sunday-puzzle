import Data.List(sort)
import Data.Set(Set,delete,elemAt,elems,findIndex,fromList,insert,size)

parse :: String -> Set (Rational,Int)
parse = fromList . zip [0..] . map read . words

mix :: Set (Rational,Int) -> ((Rational,Int),Set (Rational,Int))
mix list = foldl move1 ((0,0),list) $ elems list

move1 :: ((Rational,Int),Set (Rational,Int)) -> (Rational,Int) -> ((Rational,Int),Set (Rational,Int))
move1 (zero,list) elt@(_,n) = (newZero,insert newElt $ delete elt list)
  where
    i = findIndex elt list
    predIndex = (i+n-1) `mod` (size list - 1)
    succIndex = (i+n) `mod` (size list - 1)
    predAt = predIndex + if predIndex >= i then 1 else 0
    succAt = succIndex + if succIndex >= i then 1 else 0
    (predI,_) = elemAt predAt list
    (succI,_) = elemAt succAt list
    newElt
      | succAt == 0 = (succI/2,n)
      | otherwise = ((predI+succI)/2,n)
    newZero | n == 0 = newElt | otherwise = zero

at :: ((Rational,Int),Set (Rational,Int)) -> Int -> Int
at (zero,list) i = snd $ elemAt ((findIndex zero list+i) `mod` size list) list

testData :: String
testData = unlines [
    "1",
    "2",
    "-3",
    "3",
    "-2",
    "0",
    "4"
    ]

test :: ()
test
  | (at result 1000,at result 2000,at result 3000) /= (4,-3,2) = error "a"
  | otherwise = ()
  where result = mix $ parse testData

part1 :: IO Int
part1 = do
    result <- fmap (mix . parse) $ readFile "input/20.txt"
    return $ at result 1000 + at result 2000 + at result 3000

parse2 :: String -> Set (Rational,Int,Int)
parse2 = fromList . zip3 [0..] [0..] . map ((* 811589153) . read) . words

mix2 :: ((Rational,Int,Int),Set (Rational,Int,Int)) -> ((Rational,Int,Int),Set (Rational,Int,Int))
mix2 (_,list) = foldl move2 ((0,0,0),list) $ sort $ map toMixOrder $ elems list
  where
    toMixOrder (listOrder,mixOrder,n) = (mixOrder,listOrder,n)

move2 :: ((Rational,Int,Int),Set (Rational,Int,Int)) -> (Int,Rational,Int) -> ((Rational,Int,Int),Set (Rational,Int,Int))
move2 (zero,list) (mixOrder,listOrder,n) = (newZero,insert newElt $ delete elt list)
  where
    elt = (listOrder,mixOrder,n)
    i = findIndex elt list
    predIndex = (i+n-1) `mod` (size list - 1)
    succIndex = (i+n) `mod` (size list - 1)
    predAt = predIndex + if predIndex >= i then 1 else 0
    succAt = succIndex + if succIndex >= i then 1 else 0
    (predI,_,_) = elemAt predAt list
    (succI,_,_) = elemAt succAt list
    newElt
      | succAt < predAt = (succI/2,mixOrder,n)
      | otherwise = ((predI+succI)/2,mixOrder,n)
    newZero | n == 0 = newElt | otherwise = zero

at2 :: ((Rational,Int,Int),Set (Rational,Int,Int)) -> Int -> Int
at2 (zero,list) i = third $ elemAt ((findIndex zero list+i) `mod` size list) list
  where third (_,_,n) = n

test2 :: ()
test2
  | (at2 result 1000,at2 result 2000,at2 result 3000) /= (811589153,2434767459,-1623178306) = error "a"
  | otherwise = ()
  where result = head $ drop 10 $ iterate mix2 $ (,) (0,0,0) $ parse2 testData

part2 :: IO Int
part2 = do
    result <- fmap (head . drop 10 . iterate mix2 . (,) (0,0,0) . parse2) $ readFile "input/20.txt"
    return $ at2 result 1000 + at2 result 2000 + at2 result 3000
