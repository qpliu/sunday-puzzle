{-
--- Day 18: Operation Order ---

As you look out the window and notice a heavily-forested continent slowly
appear over the horizon, you are interrupted by the child sitting next to you.
They're curious if you could help them with their math homework.

Unfortunately, it seems like this "math" follows different rules than you
remember.

The homework (your puzzle input) consists of a series of expressions that
consist of addition (+), multiplication (*), and parentheses ((...)). Just like
normal math, parentheses indicate that the expression inside must be evaluated
before it can be used by the surrounding expression. Addition still finds the
sum of the numbers on both sides of the operator, and multiplication still
finds the product.

However, the rules of operator precedence have changed. Rather than evaluating
multiplication before addition, the operators have the same precedence, and
are evaluated left-to-right regardless of the order in which they appear.

For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are as
follows:

| 1 + 2 * 3 + 4 * 5 + 6
|   3   * 3 + 4 * 5 + 6
|       9   + 4 * 5 + 6
|          13   * 5 + 6
|              65   + 6
|                  71

Parentheses can override this order; for example, here is what happens if
parentheses are added to form 1 + (2 * 3) + (4 * (5 + 6)):

| 1 + (2 * 3) + (4 * (5 + 6))
| 1 +    6    + (4 * (5 + 6))
|      7      + (4 * (5 + 6))
|      7      + (4 *   11   )
|      7      +     44
|             51

Here are a few more examples:

 - 2 * 3 + (4 * 5) becomes 26.
 - 5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 437.
 - 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 12240.
 - ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 13632.

Before you can help with the homework, you need to understand it yourself.
Evaluate the expression on each line of the homework; what is the sum of the
resulting values?
-}

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
