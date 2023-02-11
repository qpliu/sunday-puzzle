fa :: [(Integer,Integer)]
fa = [(a*(a+1)*(a+2),a) | a <- [0..]]

fb :: [(Integer,Integer)]
fb = [(b*b+4,b) | b <- [0,2..]]

search :: [(Integer,Integer)] -> [(Integer,Integer)] -> [Either (Integer,Integer) (Integer,Integer)]
search fas@((fa,a):fas1) fbs@((fb,b):fbs1)
  | fa == fb = Right (a,b) : search fas1 fbs1
  | fa > fb = if b `mod` 1000000 == 0 then Left (a,b) : search fas fbs1 else search fas fbs1
  | otherwise = if a `mod` 1000000 == 0 then Left (a,b) : search fas1 fbs else search fas1 fbs
