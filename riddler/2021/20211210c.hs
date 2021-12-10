import Data.Map(alter,empty,toList)

leg :: Int -> Int -> Int -> Rational -> Rational -> [((Int,Int),Rational)]
leg goalPoints teamStartingPoints opponentStartingPoints winPointChance startingChance =
    -- should be some sort of binomial distribution, but the different
    -- number of points in the different cases makes it easier to just
    -- brute force it
    point [((teamStartingPoints,opponentStartingPoints),startingChance)]
  where
    point results
      | all done results = results
      | otherwise = point $ collect $ concatMap p results
    w = winPointChance
    l = 1-winPointChance
    p r@((team,opponent),chance)
      | done r = [r]
      | otherwise = [((team+1,opponent),w*chance),((team,opponent+1),l*chance)]
    done ((team,opponent),_) = team >= goalPoints || opponent >= goalPoints

collect :: Ord a => [(a,Rational)] -> [(a,Rational)]
collect items = toList $ foldl add empty items
  where add m (key,value) = alter (Just . maybe value (value+)) key m

relay :: [(Int,Rational)] -> [((Int,Int),Rational)]
relay legs = r legs [((0,0),1)]
  where
    r [] results = results
    r ((goalPoints,winPointChance):legs) results = r legs $ collect $ concatMap doLeg results
      where
        doLeg ((teamStartingPoints,opponentStartingPoints),startingChance) =
          leg goalPoints teamStartingPoints opponentStartingPoints winPointChance startingChance

winChance :: [((Int,Int),Rational)] -> Rational
winChance results = sum $ map getChances results
  where
    getChances ((teamPoints,opponentPoints),chances)
      | teamPoints > opponentPoints = chances
      | otherwise = 0

main :: IO ()
main = do
    print $ r [(15,3/4),(30,1/4),(45,1/2)]
    print $ r [(15,3/4),(30,1/2),(45,1/4)]
    print $ r [(15,1/4),(30,3/4),(45,1/2)]
    print $ r [(15,1/4),(30,1/2),(45,3/4)]
    print $ r [(15,1/2),(30,3/4),(45,1/4)]
    print $ r [(15,1/2),(30,1/4),(45,3/4)]
  where
    r legs = let c = winChance (relay legs) in (legs,c,fromRational c :: Double)
