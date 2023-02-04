import Data.Char(isDigit)
import Data.Set(fromList,size)

meetCriteria :: Int -> Bool
meetCriteria n
  | n < 100000 || n > 999999 = False
  | otherwise = ok (n `mod` 10) (n `div` 10) False
  where
    ok digit i hasDouble
      | i == 0 = hasDouble
      | digit < lastDigit = False
      | digit == lastDigit = ok lastDigit (i `div` 10) True
      | otherwise = ok lastDigit (i `div` 10) hasDouble
      where lastDigit = i `mod` 10

test :: ()
test
  | not (meetCriteria 111111) = error "a"
  | meetCriteria 223450 = error "b"
  | meetCriteria 123789 = error "c"
  | otherwise = ()

part1 :: Int -> Int -> Int
part1 a b = length $ filter meetCriteria [a..b]

meetCriteria2 :: Int -> Bool
meetCriteria2 n
  | n < 100000 || n > 999999 = False
  | otherwise = ok 1 (n `mod` 10) (n `div` 10) False
  where
    ok digitCount digit i hasDouble
      | i == 0 = hasDouble || digitCount == 2
      | digit < lastDigit = False
      | digit == lastDigit = ok (digitCount+1) lastDigit (i `div` 10) hasDouble
      | otherwise = ok 1 lastDigit (i `div` 10) (hasDouble || digitCount == 2)
      where lastDigit = i `mod` 10

test2 :: ()
test2
  | not (meetCriteria2 112233) = error "a"
  | meetCriteria2 123444 = error "b"
  | not (meetCriteria2 111122) = error "c"
  | otherwise = ()

part2 :: Int -> Int -> Int
part2 a b = length $ filter meetCriteria2 [a..b]
