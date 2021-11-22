import Data.Map(Map,alter,empty,toList)

data Hat = R | Y | B | NoGuess deriving (Eq,Ord,Show)

type Strategy = Hat -> Hat -> Hat

test :: Strategy -> Strategy -> Strategy -> Strategy -> [(Hat,Hat,Hat,Hat)]
test ns es ss ws = [(n,e,s,w) | n <- [R,Y,B], e <- [R,Y,B], s <- [R,Y,B], w <- [R,Y,B], n /= ns e w && e /= es s n && s /= ss w e && w /= ws n s]

noguess :: Strategy
noguess _ _ = NoGuess

strategyE1 :: Strategy
strategyE1 s n
  | s == R || n == R = R
  | s == Y || n == Y = Y
  | otherwise = B

strategyW1 :: Strategy
strategyW1 n s
  | n == B || s == B = B
  | n == Y || s == Y = Y
  | otherwise = R

strategyW2 :: Strategy
strategyW2 n s
  | n == Y || s == Y = Y
  | n == B || s == B = B
  | otherwise = R

strategyW3 :: Strategy
strategyW3 n s
  | n == s = R
  | n > s = Y
  | otherwise = B

strategyW4 :: Strategy
strategyW4 n s
  | n == s = R
  | n == R = Y
  | otherwise = B

gather :: [(Hat,Hat,Hat,Hat)] -> Map (Hat,Hat) [(Hat,Hat)]
gather = foldl collect empty
  where
    collect m (n,e,s,w) = alter (Just . maybe [(n,s)] ((n,s):)) (e,w) m

guessable :: [(Hat,Hat)] -> (Bool,[(Hat,Hat)])
guessable cases = (or [all (ok a b) cases | a <- [R,Y,B], b <- [R,Y,B]],cases)
  where
    ok a b (c,d) = a == c || b == d

check :: Strategy -> Strategy -> Strategy -> Strategy -> IO ()
check ns es ss ws = mapM_ (print . fmap guessable) $ toList $ gather $ test ns es ss ws
