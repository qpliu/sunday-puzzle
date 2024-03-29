import Data.List(nub,permutations)

combos :: Int -> [(Int,[Int])]
combos 0 = [(0,[])]
combos n = concat [map (addCount . (r:)) (combos' (n-1) r) | r <- [1..6]]
  where
    combos' :: Int -> Int -> [[Int]]
    combos' 0 _ = [[]]
    combos' n' m = concat [map (r:) (combos' (n'-1) r) | r <- [1..m]]
    addCount d = ((length . nub . permutations) d,d)

-- There's certainly some elegant expression from combinatorics that would
-- make this much faster but an ugly brute force should suffice
s :: Int -> Rational
s 0 = 0
s 1 = 21/6
s n = sum [fromIntegral c * sn n d | (c,d) <- combos n] / (6^fromIntegral n)

sn :: Int -> [Int] -> Rational
sn n (d1:d) = fromIntegral d1 + sn' (n-1) d
  where
    sn' _ [] = 0
    sn' n' (d1':d') = max (fromIntegral d1' + sn' (n'-1) d') (s n')

main :: IO ()
main = mapM_ (print . s) [1..5]
