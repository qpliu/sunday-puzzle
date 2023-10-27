p :: Int -> Int -> Rational
p ncand nvote = sum [choose nvote m*(n-1)^(nvote-m) | m <- [(nvote+2)`div`2..nvote]]/n^(nvote-1)
  where n = fromIntegral ncand

choose :: Int -> Int -> Rational
choose n k = product [fromIntegral k+1..fromIntegral n]/product [1..fromIntegral (n-k)]

r :: Int -> Int -> Rational
r ncand nvote
  | ncand <= 1 = 1
  | otherwise = let pp = p ncand nvote in pp + (1-pp)*(1 + r (ncand-1) nvote)
