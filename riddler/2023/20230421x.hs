import Data.Set(Set,delete,elems,fromList)

ways :: Int -> Int
ways n = sum [w i (fromList [1..n]) | i <- [1..n]]

w :: Int -> Set Int -> Int
w i slots
  | [i] == elems slots = 1
  | otherwise = sum [w j (delete i slots) | j <- elems slots, j < i-1 || j > i+1]
