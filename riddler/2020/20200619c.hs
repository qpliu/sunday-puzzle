
division :: [Integer] -> Integer -> Maybe [[Integer]]
division items groups
  | groups < 1 = Nothing
  | groups == 1 = Just [items]
  | subtotal * groups /= sum items = Nothing
  | maximum items > subtotal = Nothing
  | otherwise = search [maximum items] (filter (/= maximum items) items) []
  where
    subtotal = sum items `div` groups
    search subitems candidates disqualified
      | sum subitems == subtotal = fmap (subitems:) (division (candidates++disqualified) (groups-1))
      | null candidates = Nothing
      | sum subitems + head candidates > subtotal = search subitems (tail candidates) (head candidates:disqualified)
      | otherwise = maybe (search subitems (tail candidates) (head candidates:disqualified)) Just (search (head candidates:subitems) (tail candidates) disqualified)

findMinimum :: Integer -> Integer
findMinimum groups = search [1] 1
  where
    search items n = maybe (search ((n+1)^3:items) (n+1)) (const n) (division items groups)

main :: IO ()
main = mapM_ print [(n,findMinimum n) | n <- [2..6]]
