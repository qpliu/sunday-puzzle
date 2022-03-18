import Data.Map(Map,adjust,foldWithKey,fromList,toList,(!))

empty :: Map Integer Rational
empty = fromList [(nMonths,0) | nMonths <- [0..12]]

initial :: Map Integer Rational
initial = fromList ((0,1):[(nMonths,0) | nMonths <- [1..12]])

-- map is from number of months with at least 1 birthday to the chances
-- for that case
addPerson :: Map Integer Rational -> Map Integer Rational
addPerson m = foldWithKey add empty m
  where
    add nMonths chance m
      | nMonths == 0 = adjust (+chance) 1 m
      | nMonths == 12 = adjust (+chance) 12 m
      | otherwise =
          adjust (+(chance*fromIntegral nMonths/12)) nMonths $
          adjust (+(chance*fromIntegral (12-nMonths)/12)) (nMonths+1) m

main :: IO ()
main = do
  let c = head $ drop 40 $ iterate addPerson initial
  print $ sum c
  mapM_ print $ toList c
  print $ 1 - (c!12)
  print $ (fromRational (1 - (c!12)) :: Double)
