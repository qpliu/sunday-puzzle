import Data.List(permutations)

selfDraw :: Int -> [Int] -> Bool
selfDraw _ [] = False
selfDraw n (x:xs) | n == x = True | otherwise = selfDraw (n+1) xs

main :: IO ()
main =
  let all120 = permutations [1..5]
      noSelfDraw = filter (not . selfDraw 1) all120
  in  print (length all120,length noSelfDraw,(fromIntegral (length noSelfDraw))/(fromIntegral (length all120)) :: Double)
