import Data.List(sort)

stretch :: Int -> Int -> Int
stretch a b = check a b
  where
    check nexta nextb
      | nexta == nextb || nexta == nextb + 1 = nexta
      | nexta + 1 == nextb = nextb
      | nexta > nextb = check nexta (nextb + b)
      | otherwise = check (nexta + a) nextb

main :: IO ()
main = mapM_ print (sort [(stretch a b,a,b) | a <- [1..20], b <- [a..20], gcd a b == 1])
