import Data.Map(fromList,(!))

loseProbability :: Int -> Int -> Int -> Int -> Rational
loseProbability cards ones twos threes = table!(cards,ones,twos,threes)
  where
    table = fromList [((ncards,nones,ntwos,nthrees),loseProb ncards nones ntwos nthrees) | ncards <- [0..cards], nones <- [0..ones], ntwos <- [0..twos], nthrees <- [0..threes]]
    loseProb ncards nones ntwos nthrees
      | nones == 0 && ntwos == 0 && nthrees == 0 = 0
      | ncards < nones + ntwos + nthrees = 0
      | ncards `mod` 3 == 1 = -- ones lose
            n1/nc + flipped2 + flipped3 + flippedOther
      | ncards `mod` 3 == 0 = -- twos lose
            flipped1 + n2/nc + flipped3 + flippedOther
      | ncards `mod` 3 == 2 = -- threes lose
            flipped1 + flipped2 + n3/nc + flippedOther
      where
        nc = fromIntegral ncards
        n1 = fromIntegral nones
        n2 = fromIntegral ntwos
        n3 = fromIntegral nthrees
        flipped1
          | nones > 0 = n1/nc*table!(ncards-1,nones-1,ntwos,nthrees)
          | otherwise = 0
        flipped2
          | ntwos > 0 = n2/nc*table!(ncards-1,nones,ntwos-1,nthrees)
          | otherwise = 0
        flipped3
          | nthrees > 0 = n3/nc*table!(ncards-1,nones,ntwos,nthrees-1)
          | otherwise = 0
        flippedOther = (nc-n1-n2-n3)/nc*table!(ncards-1,nones,ntwos,nthrees)

main :: IO ()
main = print (winProb,fromRational winProb :: Double)
  where winProb = 1 - loseProbability 40 4 4 4
