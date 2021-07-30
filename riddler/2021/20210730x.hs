import Data.Map(Map,alter,empty,toList)

addShot1 :: (Int,Rational) -> [(Int,Rational)]
addShot1 (score,odds) = [(score+10,odds/3),(score+9,odds/3),(score+5,odds/3)]

addShot :: [(Int,Rational)] -> [(Int,Rational)]
addShot states = toList (foldl collect empty (concatMap addShot1 states))
  where
    collect m (score,odds) = alter (Just . maybe odds (odds+)) score m

addRound :: [(Int,Rational)] -> [(Int,Rational)]
addRound = addShot . addShot . addShot

main :: IO ()
main = do
    let tiebreak = addRound [(0,1)]
        tbwin = sum (map snd (filter ((> 24) . fst) tiebreak))
        tblose = sum (map snd (filter ((< 24) . fst) tiebreak))
        tbtie = sum (map snd (filter ((== 24) . fst) tiebreak))
    print (tbwin,tblose,tbtie)
    print (tbwin/(tbwin+tblose),tblose/(tbwin+tblose))
    let results = addRound (addRound (addRound [(0,1)]))
        win = sum (map snd (filter ((> 72) . fst) results))
        lose = sum (map snd (filter ((< 72) . fst) results))
        tie = sum (map snd (filter ((== 72) . fst) results))
    print (win,lose,tie)
    print (win+tie*tbwin/(tbwin+tblose),lose+tie*tblose/(tbwin+tblose))
