ways :: Int -> Int
ways pts = sum [waysFG tds (pts - tds*6) | tds <- [0..pts `div` 6]]

waysFG :: Int -> Int -> Int
waysFG tds pts = sum [waysPAT tds (pts - 3*fgs) | fgs <- [0..pts `div` 3]]

waysPAT :: Int -> Int -> Int
waysPAT tds pts = sum [ways2PT (tds-pat) (pts-pat) | pat <- [0..min pts tds]]

ways2PT :: Int -> Int -> Int
ways2PT tds pts = sum [waysSAF (pts-2*x2) | x2 <- [0..min (pts `div` 2) tds]]

waysSAF :: Int -> Int
waysSAF pts
  | odd pts = 0
  | otherwise = 1 -- all safeties
