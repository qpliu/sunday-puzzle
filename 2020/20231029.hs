rot :: Char -> Char
rot '0' = '0'
rot '1' = '1'
rot '2' = '2'
rot '5' = '5'
rot '6' = '9'
rot '8' = '8'
rot '9' = '6'
rot _ = '*'

ud :: String -> Bool
ud s = s == reverse (map rot s)

main :: IO ()
main = do
  print $ (2*) $ length $ filter ud [show hr ++ drop 1 (show (100+min)) | hr <- [1..12], min <- [0..59]]
  print $ length $ filter ud [drop 1 (show (100+hr)) ++ drop 1 (show (100+min)) | hr <- [0..23], min <- [0..59]]
