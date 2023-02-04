import Data.Char(isDigit,isSpace)

eval :: String -> Int
eval = fst . evalLHS . filter (not . isSpace)

evalLHS :: String -> (Int,String)
evalLHS expr
  | take 1 expr == "(" = let (x,rest) = evalLHS (tail expr) in evalOP x rest
  | null expr || not (isDigit (head expr)) = error expr
  | otherwise = let (x,rest) = span isDigit expr in evalOP (read x) rest

evalOP :: Int -> String -> (Int,String)
evalOP x expr
  | null expr || take 1 expr == ")" = (x,drop 1 expr)
  | head expr == '+' = evalRHS (x+) (tail expr)
  | head expr == '*' = evalRHS (x*) (tail expr)
  | otherwise = error expr

evalRHS :: (Int -> Int) -> String -> (Int,String)
evalRHS op expr
  | take 1 expr == "(" =
      let (y,rest) = evalLHS (tail expr) in evalOP (op y) rest
  | null expr || not (isDigit (head expr)) = error expr
  | otherwise = let (y,rest) = span isDigit expr in evalOP (op (read y)) rest

test :: ()
test
  | eval "1 + 2 * 3 + 4 * 5 + 6" /= 71 = error "a"
  | eval "1 + (2 * 3) + (4 * (5 + 6))" /= 51 = error "b"
  | eval "2 * 3 + (4 * 5)" /= 26 = error "c"
  | eval "5 + (8 * 3 + 9 + 3 * 4 * 3)" /= 437 = error "d"
  | eval "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" /= 12240 = error "e"
  | eval "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" /= 13632 = error "f"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . map eval . lines) $ readFile "input/18.txt"

eval2 :: String -> Int
eval2 = fst . evalLHS2 . filter (not . isSpace)

evalLHS2 :: String -> (Int,String)
evalLHS2 expr
  | take 1 expr == "(" = let (x,rest) = evalLHS2 (tail expr) in evalOP2 [] x rest
  | null expr || not (isDigit (head expr)) = error expr
  | otherwise = let (x,rest) = span isDigit expr in evalOP2 [] (read x) rest

evalOP2 :: [Int -> Int] -> Int -> String -> (Int,String)
evalOP2 stack x expr
  | null expr || take 1 expr == ")" = (foldr ($) x stack,drop 1 expr)
  | head expr == '+' = evalRHS2 stack (x+) (tail expr)
  | head expr == '*' = evalRHS2 [(* foldr ($) x stack)] id (tail expr)
  | otherwise = error expr

evalRHS2 :: [Int -> Int] -> (Int -> Int) -> String -> (Int,String)
evalRHS2 stack op expr
  | take 1 expr == "(" =
      let (y,rest) = evalLHS2 (tail expr) in evalOP2 stack (op y) rest
  | null expr || not (isDigit (head expr)) = error expr
  | otherwise =
      let (y,rest) = span isDigit expr in evalOP2 stack (op (read y)) rest

test2 :: ()
test2
  | eval2 "1 + 2 * 3 + 4 * 5 + 6" /= 231 = error "a"
  | eval2 "1 + (2 * 3) + (4 * (5 + 6))" /= 51 = error "b"
  | eval2 "2 * 3 + (4 * 5)" /= 46 = error "c"
  | eval2 "5 + (8 * 3 + 9 + 3 * 4 * 3)" /= 1445 = error "d"
  | eval2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" /= 669060 = error "e"
  | eval2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" /= 23340 = error "f"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (sum . map eval2 . lines) $ readFile "input/18.txt"
