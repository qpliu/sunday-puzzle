import Data.Set(fromList,size)

firstFolds :: Int -> [[[Int]]]
firstFolds n =
    [[[(i-j)`mod`(2*n),(i+j+1)`mod`(2*n)] | j <- [0..n-1]] | i <- [0..n-1]]

foldAt :: Int -> Bool -> [[Int]] -> [[Int]]
foldAt i up p =
    takeWhile (not . null) $ if up then zipWith (++) aa bb
                                   else zipWith (++) bb aa
  where
    (a,b) = splitAt i p
    aa = reverse a ++ repeat []
    bb = map reverse b ++ repeat []

foldPackets :: [[Int]] -> [[Int]]
foldPackets [p]
  | head p == 0 = [p]
  | last p == 0 = [reverse p]
  | otherwise = []
foldPackets p
  | 0 `elem` (map head p ++ map last p) =
    concatMap foldPackets [foldAt i up p | i <- [1..length p-1],
                                           up <- [True, False]]
  | otherwise = []

main :: IO ()
main =
    print $ map (size . fromList . concatMap foldPackets . firstFolds) [2..11]
