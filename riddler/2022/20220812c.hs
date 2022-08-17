bet :: Double -> Double -> Double -> (Double,Double) -> (Double,Double)
bet goal house fraction (previousSuccess,money) =
    (previousSuccess+thisSuccess,money*(1-fraction))
  where
    thisSuccess = (1-previousSuccess)*house*fraction*money/(goal-money*(1-fraction))
