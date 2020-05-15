import Data.Ratio((%))

fair :: Integer -> Integer -> Rational
fair sides n
  | n >= 1 && n <= sides = 1 % sides
  | otherwise = error (show n)

adv :: (Integer -> Integer -> Rational) -> Integer -> Integer -> Rational
adv p sides n
  | n >= 1 && n <= sides = (p sides n)*(p sides n + 2*sum [p sides i | i <- [1..n-1]])
  | otherwise = error (show n)

dis :: (Integer -> Integer -> Rational) -> Integer -> Integer -> Rational
dis p sides n
  | n >= 1 && n <= sides = (p sides n)*(p sides n + 2*sum [p sides i | i <- [n+1..sides]])
  | otherwise = error (show n)

check :: (Integer -> Integer -> Rational) -> Integer -> Bool
check p sides = sum [p sides n | n <- [1..sides]] == toRational (1 % 1)

expected :: (Integer -> Integer -> Rational) -> Integer -> Rational
expected p sides = sum [fromIntegral n * p sides n | n <- [1..sides]]

win ::  (Integer -> Integer -> Rational) -> Integer -> Integer -> Rational
win p sides n = sum [p sides i | i <- [n..sides]]

main :: IO ()
main = do
    exp "single roll" fair
    exp "roll with advantage" (adv fair)
    exp "roll with disadvantage" (dis fair)
    exp "roll with advantage of disadvantage" (adv (dis fair))
    exp "roll with disadvantage of advantage" (dis (adv fair))
    mapM_ winner [1..20]
  where
    exp name p = putStrLn ("Expected value for "++name++": "++show (expected p 20)++","++show (fromRational (expected p 20) :: Double))
    winner n = putStrLn ("Chance of rolling at least "++show n++" single roll:"++show (s,s==highest,fromRational s :: Double)++" AofD: "++show (aofd,aofd==highest,fromRational aofd :: Double)++" DofA: "++show (dofa,dofa==highest,fromRational dofa :: Double))
      where
        s = win fair 20 n
        aofd = win (adv (dis fair)) 20 n
        dofa = win (dis (adv fair)) 20 n
        highest = maximum [s,aofd,dofa]

retroactiveDofA :: [(Integer,Rational)]
retroactiveDofA = [(n,wins n) | n <- [1..sides]]
  where
    sides = 20
    wins n = singleWins n + singleLoses n*retroDofA n
    singleWins n = sum [fair sides i | i <- [n..sides]]
    singleLoses n = toRational (1 % 1) - singleWins n
    retroDofA n = singleWins n * win (adv fair) sides n
