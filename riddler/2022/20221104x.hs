main :: IO ()
main = print $ minimum [(abs (l/n - 24217/100000),(n,l)) | n <- [1..400], l <- map fromIntegral [floor (n*24217/100000) .. floor (n*24217/100000)+1]]
