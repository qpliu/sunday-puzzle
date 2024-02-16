revSeqs :: Int -> [[Int]]
revSeqs n = concat [makeRevSeqs n [i] [i] [([i],0)] | i <- [1..n]]

makeRevSeqs :: Int -> [Int] -> [Int] -> [([Int],Int)] -> [[Int]]
makeRevSeqs n revSeq sums pivots
  | null continued = [revSeq]
  | otherwise = continued
  where
    newSums i = i : map (i +) sums
    newPivots i = (newSums i,0) : map (fmap (i +)) pivots
    valid i (precedingSums,pivotSum) = not $ elem (pivotSum+i) precedingSums
    continued = concat [makeRevSeqs n (i:revSeq) (newSums i) (newPivots i) | i <- [1..n], all (valid i) pivots]
