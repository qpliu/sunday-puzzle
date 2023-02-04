import Data.Char(isDigit,ord)

parse :: String -> [Int]
parse s = map ((+ (-ord '0')) . ord) $ filter isDigit $ s

captcha :: [Int] -> Int
captcha digits = fst $ foldr scan (0,head digits) digits
  where
    scan digit (runningTotal,nextDigit)
      | digit == nextDigit = (digit+runningTotal,digit)
      | otherwise = (runningTotal,digit)

test :: ()
test
  | captcha (parse "1122") /= 3 = error "a"
  | captcha (parse "1111") /= 4 = error "b"
  | captcha (parse "1234") /= 0 = error "c"
  | captcha (parse "91212129") /= 9 = error "d"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (captcha . parse) $ readFile "input/01.txt"

captcha2 :: [Int] -> Int
captcha2 digits = sum $ map fst $ filter (uncurry (==)) $ zip digits (right++left)
  where (left,right) = splitAt (length digits `div` 2) digits

test2 :: ()
test2
  | captcha2 (parse "1212") /= 6 = error "a"
  | captcha2 (parse "1221") /= 0 = error "b"
  | captcha2 (parse "123425") /= 4 = error "c"
  | captcha2 (parse "123123") /= 12 = error "d"
  | captcha2 (parse "12131415") /= 4 = error "e"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (captcha2 . parse) $ readFile "input/01.txt"
