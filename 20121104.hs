main = readFile "/usr/share/dict/words" >>= mapM_ putStrLn . filter f . lines
  where
    f word = length word == 7 && f' "organic" word == 5 && f' "natural" word == 5 && f' word "organic" == 5 && f' word "natural" == 5
    f' w1 w2 = length (filter (`elem` w1) w2)
