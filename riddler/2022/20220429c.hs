import Data.Map(Map,alter,empty,fromList,mapKeys,mapWithKey,mergeWithKey,toList)

newtype Chance = Chance (Integer,Integer,Integer) deriving (Eq,Ord,Show)

tied :: Chance
tied = Chance (1,0,0) -- 1/2

ahead :: Chance
ahead = Chance (0,1,0) -- 1/2-x

behind :: Chance
behind = Chance (0,0,1) -- 1/2+x

(!*) :: Chance -> Chance -> Chance
(!*) (Chance (a,b,c)) (Chance (x,y,z)) = Chance (a+x,b+y,c+z)

newtype Chances = Chances (Map Chance Integer) deriving (Eq,Ord,Show)

(@*) :: Chance -> Chances -> Chances
(@*) chance (Chances chances) = Chances (mapKeys (chance !*) chances)

(@+) :: Chances -> Chances -> Chances
(@+) (Chances a) (Chances b) =
    Chances (mergeWithKey (\ _ x y -> Just (x + y)) id id a b)

newtype Scores = Scores (Map Int Chances) deriving (Eq,Ord,Show)

nicksPossession :: Scores -> Scores
nicksPossession (Scores scores) =
    Scores (mergeWithKey (\ _ x y -> Just (x @+ y)) id id
                (fromList (map score (toList scores)))
                (fromList (map failToScore (toList scores))))
  where
    score (diff,chances)
      | diff == 0 = (diff+2,tied @* chances)
      | diff > 0  = (diff+2,ahead @* chances)
      | otherwise = (diff+2,behind @* chances)
    failToScore (diff,chances)
      | diff == 0 = (diff,tied @* chances) -- tied == 1 - tied
      | diff > 0  = (diff,behind @* chances) -- behind = 1 - ahead
      | otherwise = (diff,ahead @* chances) -- ahead = 1 - behind

naughtsPossession :: Scores -> Scores
naughtsPossession (Scores scores) =
    Scores (mergeWithKey (\ _ x y -> Just (x @+ y)) id id
                (fromList (map score (toList scores)))
                (fromList (map failToScore (toList scores))))
  where
    score (diff,chances)
      | diff == 0 = (diff-2,tied @* chances)
      | diff < 0  = (diff-2,ahead @* chances)
      | otherwise = (diff-2,behind @* chances)
    failToScore (diff,chances)
      | diff == 0 = (diff,tied @* chances) -- tied == 1 - tied
      | diff < 0  = (diff,behind @* chances) -- behind = 1 - ahead
      | otherwise = (diff,ahead @* chances) -- ahead = 1 - behind

possession :: Scores -> Scores
possession = naughtsPossession . nicksPossession

start :: Scores
start = Scores (fromList [(0,Chances (fromList [(Chance (0,0,0),1)]))])

addChances :: (Int -> Bool) -> Scores -> Chances
addChances hasDiff (Scores scores) = foldl add (Chances empty) (toList scores)
  where
    add runningTotal (diff,chance)
      | hasDiff diff = runningTotal @+ chance
      | otherwise = runningTotal

calculateChance :: Rational -> Chances -> Rational
calculateChance x (Chances chances) = foldl add 0 (toList chances)
  where
    a = 1 - x/50
    b = 2 - a
    add runningTotal (Chance (n,na,nb),c) =
        runningTotal + fromIntegral c*a^na*b^nb/2^(n+na+nb)

main :: IO ()
main = do
    p "tie,x=0" (calculateChance 0 tie)
    p "tie,x=50" (calculateChance 50 tie)
    p "tie-1/2,x=25" (calculateChance 25 tie - 1/2)
    p "tie-1/2,x=24.9999999999999743" (calculateChance (24 + 9999999999999743/10000000000000000) tie - 1/2)
    p "tie-1/2,x=24.9999999999999742" (calculateChance (24 + 9999999999999742/10000000000000000) tie - 1/2)
    let x = (24 + 9999999999999743/10000000000000000) in p "nyWin/(1-tie),x=24.9999999999999743" (calculateChance x nyWin/(1 - calculateChance x tie))
    let x = (24 + 9999999999999743/10000000000000000) in p "bknWin/(1-tie),x=24.9999999999999743" (calculateChance x bknWin/(1 - calculateChance x tie))
    let x = (24 + 9999999999999742/10000000000000000) in p "nyWin/(1-tie),x=24.9999999999999742" (calculateChance x nyWin/(1 - calculateChance x tie))
    let x = (24 + 9999999999999742/10000000000000000) in p "bknWin/(1-tie),x=24.9999999999999742" (calculateChance x bknWin/(1 - calculateChance x tie))
  where
    outcomes = head $ drop 100 $ iterate possession start
    tie = addChances (== 0) outcomes
    nyWin = addChances (> 0) outcomes
    bknWin = addChances (< 0) outcomes
    p msg v = print (msg,v,fromRational v :: Double)
