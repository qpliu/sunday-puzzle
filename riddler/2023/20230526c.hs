orderings :: Int -> Int -> [String]
orderings nleave ndriveby
  | nleave <= 0 = [take ndriveby $ repeat 'D']
  | ndriveby <= 0 = [take nleave $ repeat 'L']
  | otherwise = map ('L':) (orderings (nleave-1) ndriveby) ++ map ('D':) (orderings nleave (ndriveby-1))

parkedstragglers :: String -> Int
parkedstragglers ordering = s ordering 0
  where
    s "" _ = 0
    s ('D':ordering) open
      | open <= 0 = s ordering 0
      | otherwise = 1 + s ordering (open-1)
    s ('L':ordering) open = s ordering (open+1)

main :: IO ()
main = do
    mapM_ print scenarios
    print (prob :: Rational,fromRational prob :: Double)
  where
    scenarios = [(p,length $ filter ((== p) . parkedstragglers) (orderings 3 9)) | p <- [0..3]]
    denom = sum (map snd scenarios)
    prob = sum [(fromIntegral p*fromIntegral n)/(9*fromIntegral denom) | (p,n) <- scenarios]
