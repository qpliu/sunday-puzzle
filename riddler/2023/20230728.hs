import Data.Set(fromList,size)

numbers :: (Int -> Int) -> (Int -> Int) -> (Int -> Bool) -> [Int] -> [Int]
numbers f g outOfRange [] = []
numbers f g outOfRange (n:ns)
  | outOfRange n = numbers f g outOfRange ns
  | otherwise = n : numbers f g outOfRange (f n:g n:ns)

main :: IO ()
main = do
    print $ size $ fromList $ numbers ((+ 1) . (* 2)) (* 4) (> 1024) [1]
    print $ size $ fromList $ numbers ((+ 1) . (* (-2))) (* 4) ((> 1024) . abs) [1]
