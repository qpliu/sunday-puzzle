import Data.Map(Map,alter,empty,fromList,toList)

d :: Int -> Map Int Rational
d n = fromList [(i,1/fromIntegral n) | i <- [1..n]]

d4 = d 4
d6 = d 6
d8 = d 8
d12 = d 12
d20 = d 20

combine :: Map Int Rational -> Map Int Rational -> Map Int Rational
combine as bs = foldl add empty [join a b | a <- toList as, b <- toList bs]
  where
    join (na,pa) (nb,pb) = (na+nb,pa*pb)
    add m (n,p) = alter (Just . maybe p (+p)) n m

likeliest :: Map Int Rational -> [Int]
likeliest m = map fst $ filter ((== maximum m) . snd) $ toList m

smls :: [Map Int Rational] -> Maybe Int
smls [] = Nothing
smls dice = single $ likeliest $ foldl combine (fromList [(0,1)]) dice
  where
    single [n] = Just n
    single _ = Nothing
