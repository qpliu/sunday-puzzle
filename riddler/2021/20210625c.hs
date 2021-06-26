import Data.List(delete)

score :: Int -> Int -> [Int] -> Rational
score threshold hand deck
  | hand >= threshold = fromIntegral hand
  | otherwise = sum [score threshold (hand + draw) (delete draw deck) | draw <- deck] / fromIntegral (1 + length deck)

main :: IO ()
main = print (score 28 0 [1..10],floor p,p-fromIntegral (floor p),fromRational p :: Double)
  where p = score 28 0 [1..10]