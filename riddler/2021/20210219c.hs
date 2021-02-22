import Data.Map(Map,foldlWithKey,fromList,unionsWith)

addBlock :: Double -> Int -> [Double] -> Map Int Double
addBlock delta maxHeight tower
  | height >= maxHeight = fromList [collapse,higherThanMax]
  | otherwise = unionsWith (+) (fromList [collapse] : [fmap (* (delta/2)) (addBlock delta maxHeight (d:tower)) | d <- [dmin+delta/2,dmin+3*delta/2..dmax-delta/2]])
  where
    height = length tower + 1
    dmin = maximum [ -1 - fromIntegral i - sum (zipWith (*) (take i tower) (map fromIntegral [2..])) | i <- [0..height-1]]
    dmax = minimum [ 1 + fromIntegral i - sum (zipWith (*) (take i tower) (map fromIntegral [2..])) | i <- [0..height-1]]
    collapse = (height+1,(dmin-(-1)+1-dmax)/2)
    higherThanMax = (height+2,(dmax-dmin)/2)

avgHeight :: Map Int Double -> Double
avgHeight chancesOfCollapse = foldlWithKey f 0 chancesOfCollapse
  where f x height chance = x + fromIntegral height * chance

main :: IO ()
main = do
  let chances = addBlock 0.25 6 []
  print chances
  print (avgHeight chances)
