import Data.Time(getCurrentTime)

data Hat = R | Y | B deriving (Eq,Show)

data Strategy = S Strategy1 Strategy2

data Strategy1 = S1 String (Hat -> Hat)

data Strategy2 = S2 String ((Hat,Hat) -> Hat)

instance Show Strategy where
    show (S s1 s2) = show s1 ++ "/" ++ show s2

instance Show Strategy1 where
    show (S1 name _) = name

instance Show Strategy2 where
    show (S2 name _) = name

rot :: Hat -> Hat
rot R = Y
rot Y = B
rot B = R

add :: (Hat,Hat) -> Hat
add (R,Y) = B
add (Y,B) = R
add (B,R) = Y
add (Y,R) = B
add (B,Y) = R
add (R,B) = Y
add (a,b) | a == b = a | otherwise = error "Should not happen"

baseStrategies1 :: [Strategy1]
baseStrategies1 = [S1 "I" id,S1 "C" (const R)]

baseStrategies2 :: [Strategy2]
baseStrategies2 = [S2 "+" add, S2 "L" fst, S2 "R" snd, S2 "C" (const R)]

rot1s :: Strategy1 -> [Strategy1]
rot1s s@(S1 name f) = [s,S1 ("r"++name) (rot . f),S1 ("r2"++name) (rot . rot . f)]

rot2s :: Strategy2 -> [Strategy2]
rot2s s@(S2 name f) = [s,S2 ("r"++name) (rot . f),S2 ("r2"++name) (rot . rot . f)]

strategies :: [Strategy]
strategies = [S s1 s2 | s1 <- concatMap rot1s baseStrategies1, s2 <- concatMap rot2s baseStrategies2]

strategyCombinations :: [(Strategy,Strategy,Strategy,Strategy)]
strategyCombinations = [(sn,se,ss,sw) | sn <- strategies, se <- strategies, ss <- strategies, sw <- strategies]

escapes :: (Strategy,Strategy,Strategy,Strategy) -> Bool
escapes (S (S1 _ f1n) (S2 _ f2n),S (S1 _ f1e) (S2 _ f2e),S (S1 _ f1s) (S2 _ f2s),S (S1 _ f1w) (S2 _ f2w)) =
    and [ok f1n f2n n e w || ok f1e f2e e s n || ok f1s f2s s w e || ok f1w f2w w n s | n <- [R,Y,B], e <- [R,Y,B], s <- [R,Y,B], w <- [R,Y,B]]
  where
    ok f1 f2 me left right | left == right = me == f1 left
                           | otherwise = me == f2 (left,right)

search :: Int -> Int -> [(Strategy,Strategy,Strategy,Strategy)] -> IO ()
search blockSize start combos
  | null combos = print "done"
  | otherwise = do
      getCurrentTime >>= print . ((,) start)
      let found = filter escapes (take blockSize combos) in
          if null found
              then search blockSize (start+blockSize) (drop blockSize combos)
              else print (length found,take 1 found)

main :: IO ()
main = search 1000000 0 strategyCombinations
