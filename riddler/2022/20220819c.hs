import Data.Map(Map,fromList,toList,unionsWith,(!))

p1 :: ((Int,Int),Rational) -> Map (Int,Int) Rational
p1 ((half,full),prob) = fromList $ filter (\ ((h,f),p) -> h >= 0 && f >= 0 && p > 0) [
    ((half-3,full),prob*divide (half*(half-1)*(half-2)) ((half+full)*(half+full-1)*(half+full-2))),
    ((half+1,full-2),prob*divide (full*(full-1)) ((half+full)*(half+full-1))),
    ((half-1,full-1),prob*(divide (2*half*full) ((half+full)*(half+full-1)) + if half < 2 then 0 else divide (half*(half-1)*full) ((half+full)*(half+full-1)*(half+full-2))))
    ]
  where
    divide a b
      | a == 0 && b == 0 = 1
      | otherwise = fromIntegral a/fromIntegral b

p :: Map (Int,Int) Rational -> Map (Int,Int) Rational
p ps = (unionsWith (+) . map p1 . toList) ps

main :: IO ()
main = (print . (!(1,1)) . head . drop 9 . iterate p . fromList) [((0,15),1)]
