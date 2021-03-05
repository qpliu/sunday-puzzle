hitsLowerBound :: Integer -> Rational
hitsLowerBound games = fromIntegral (2*games*games-games) / 500

hitsUpperBound :: Integer -> Rational
hitsUpperBound games = fromIntegral (2*games*games+games) / 500

hasMatchedRoundedBA :: Integer -> Bool
hasMatchedRoundedBA games =
    floor (hitsUpperBound games) > floor (hitsLowerBound games)

main :: IO ()
main = print (maximum (filter (not . hasMatchedRoundedBA) [0..1000]))
