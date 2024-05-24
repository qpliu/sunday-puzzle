import Data.List(delete,subsequences)
import Data.Map(fromList,(!))

rounds :: Int -> Int -> ((Rational,Rational),Rational)
rounds deckSize target = ((a,b),b/(1-a))
  where
    (a,b) = table![1..deckSize]
    deckTotal = sum [1..deckSize]
    table = fromList [(deck,coeffs deck) | deck <- subsequences [1..deckSize], deckTotal - sum deck <= target]
    coeffs deck
      | deckTotal - sum deck == target = (0,1) -- win
      | deckTotal - sum deck + maximum deck > target = (1,1) -- start over
      | otherwise = (sum (map fst cs)/n,sum (map snd cs)/n)
      where
        n = fromIntegral (length deck)
        cs = [table!delete draw deck | draw <- deck]
