fibs :: Int -> Int -> [Int]
fibs m n = (m+n):fibs n (m+n)

encodings :: Int -> [((Int,Int),(Int,Int))]
encodings x = filter isX [(getQ a b,(a,b)) | a <- [1..x], b <- [1..x]]
  where
    isX ((_,s),_) = s == x
    getQ a b = head $ dropWhile ((< x) . snd) $ zip [1..] $ fibs a b

main :: IO ()
main = do
  print $ encodings 81
  print $ length $ encodings 81
  print $ maximum $ encodings 81
  print $ encodings 179
  print $ length $ encodings 179
  print $ maximum $ encodings 179
