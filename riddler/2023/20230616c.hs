import Data.List(sort)
import Data.Set(Set,empty,insert,member,size)

next :: Int -> Int
next n = ((n*n) `div` 100) `mod` 10000

loop :: Set Int -> Int -> Set Int
loop set n | member n set = set | otherwise = loop (insert n set) (next n)

main :: IO ()
main = mapM_ print $ take 20 $ reverse $ sort [(size (loop empty n),n) | n <- [0..9999]]
