import Data.Bits((.&.))

gen :: Int -> Int -> Int
gen factor val = (val*factor) `mod` 2147483647

genA :: Int -> Int
genA = gen 16807

genB :: Int -> Int
genB = gen 48271

match16 :: (Int,Int) -> Bool
match16 (a,b) = a .&. 65535 == b .&. 65535

test :: ()
test
  | take 5 (tail $ iterate genA 65) /= [1092455,1181022009,245556042,1744312007,1352636452] = error "a"
  | take 5 (tail $ iterate genB 8921) /= [430625591,1233683848,1431495498,137874439,285222916] = error "b"
  | length (filter match16 $ take 5 $ tail $ zip (iterate genA 65) (iterate genB 8921)) /= 1 = error "c"
  | length (filter match16 $ take 40000000 $ tail $ zip (iterate genA 65) (iterate genB 8921)) /= 588 = error "d"
  | otherwise = ()

part1 :: Int -> Int -> Int
part1 a b = length $ filter match16 $ take 40000000 $ zip (iterate genA a) (iterate genB b)

genA2 :: Int -> [Int]
genA2 = filter ((== 0) . (`mod` 4)) . tail . iterate genA

genB2 :: Int -> [Int]
genB2 = filter ((== 0) . (`mod` 8)) . tail . iterate genB

test2 :: ()
test2
  | take 5 (genA2 65) /= [1352636452,1992081072,530830436,1980017072,740335192] = error "a"
  | take 5 (genB2 8921) /= [1233683848,862516352,1159784568,1616057672,412269392] = error "b"
  | length (filter match16 $ take 1056 $ zip (genA2 65) (genB2 8921)) /= 1 = error "c"
  | length (filter match16 $ take 5000000 $ tail $ zip (genA2 65) (genB2 8921)) /= 309 = error "d"
  | otherwise = ()

part2 :: Int -> Int -> Int
part2 a b = length $ filter match16 $ take 5000000 $ zip (genA2 a) (genB2 b)
