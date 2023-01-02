{-
--- Day 15: Dueling Generators ---

Here, you encounter a pair of dueling generators. The generators, called
generator A and generator B, are trying to agree on a sequence of numbers.
However, one of them is malfunctioning, and so the sequences don't always
match.

As they do this, a judge waits for each of them to generate its next value,
compares the lowest 16 bits of both values, and keeps track of the number of
times those parts of the values match.

The generators both work on the same principle. To create its next value, a
generator will take the previous value it produced, multiply it by a factor
(generator A uses 16807; generator B uses 48271), and then keep the remainder
of dividing that resulting product by 2147483647. That final remainder is the
value it produces next.

To calculate each generator's first value, it instead uses a specific starting
value as its "previous value" (as listed in your puzzle input).

For example, suppose that for starting values, generator A uses 65, while generator B uses 8921. Then, the first five pairs of generated values are:

| --Gen. A--  --Gen. B--
|    1092455   430625591
| 1181022009  1233683848
|  245556042  1431495498
| 1744312007   137874439
| 1352636452   285222916

In binary, these pairs are (with generator A's value first in each pair):

| 00000000000100001010101101100111
| 00011001101010101101001100110111
| 
| 01000110011001001111011100111001
| 01001001100010001000010110001000
| 
| 00001110101000101110001101001010
| 01010101010100101110001101001010
| 
| 01100111111110000001011011000111
| 00001000001101111100110000000111
| 
| 01010000100111111001100000100100
| 00010001000000000010100000000100

Here, you can see that the lowest (here, rightmost) 16 bits of the third value
match: 1110001101001010. Because of this one match, after processing these five
pairs, the judge would have added only 1 to its total.

To get a significant sample, the judge would like to consider 40 million pairs. (In the example above, the judge would eventually find a total of 588 pairs that match in their lowest 16 bits.)

After 40 million pairs, what is the judge's final count?
-}

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
