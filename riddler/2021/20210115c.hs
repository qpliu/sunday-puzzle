row :: Int -> [(Int,Int,Int)]
row prod = [(a,b,c) | a <- [1..9], b <- [1,2,4,5,7,8], c <- [1,2,3,6,7,9], a*b*c == prod]

combinations :: [[a]] -> [[a]]
combinations [] = [[]]
combinations (r:rs) = concatMap ((\ crs a -> map (a:) crs) (combinations rs)) r

hasCols :: (Int,Int,Int) -> [(Int,Int,Int)] -> Bool
hasCols prods r = prods == foldr (\ (a,b,c) (x,y,z) -> (a*x,b*y,c*z)) (1,1,1) r

main :: IO ()
main = print $ filter (hasCols (8890560,156800,55566)) $ combinations $ map row [294,216,135,98,112,84,245,40]
