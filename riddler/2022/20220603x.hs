import Data.Set(Set,delete,fromList,member,toList)

data Expr = C Int | Plus Expr Expr | Times Expr Expr deriving Show

make :: Set Int -> Int -> [(Expr,Set Int)]
make numbers target
  | null numbers = []
  | target `member` numbers = (C target,delete target numbers) : submakes
  | otherwise = submakes
  where
    submakes =
        [(Plus expr1 expr2,ns2) | l <- [1 .. target `div` 2], (expr1,ns1) <- make numbers l, (expr2,ns2) <- make ns1 (target - l)] ++
        [(Times expr1 expr2,ns2) | l <- [1 .. target `div` 2], target `mod` l == 0, (expr1,ns1) <- make numbers l, (expr2,ns2) <- make ns1 (target `div` l)]

fun n = search (sum [1..n]+1)
  where
    search i | null (make (fromList [1..n]) i) = i
             | otherwise = search (i+1)
