data State = HHH | HHT | HTH | HTT | THH | THT | TTH | TTT
    deriving (Bounded,Enum,Eq,Show)

flipH :: State -> State
flipH HHH = HHH
flipH HHT = HTH
flipH HTH = THH
flipH HTT = TTH
flipH THH = HHH
flipH THT = HTH
flipH TTH = THH
flipH TTT = TTH

flipT :: State -> State
flipT HHH = HHT
flipT HHT = HTT
flipT HTH = THT
flipT HTT = TTT
flipT THH = HHT
flipT THT = HTT
flipT TTH = THT
flipT TTT = TTT

states :: [State]
states = [minBound .. maxBound]

matrix :: State -> State -> [[Rational]]
matrix jtarget ktarget = [eqn var | var <- vars]
  where
    vars = [(j,k) | j <- states, k <- states, (j /= jtarget && k /= ktarget) || (j == jtarget && k == ktarget)]
    eqn (j0,k0) = [
        (if j == j0 && k == k0 then -1 else 0) +
        (if j == flipH j0 && k == flipH k0 then 1/4 else 0) +
        (if j == flipH j0 && k == flipT k0 then 1/4 else 0) +
        (if j == flipT j0 && k == flipH k0 then 1/4 else 0) +
        (if j == flipT j0 && k == flipT k0 then 1/4 else 0)
        | (j,k) <- vars] ++ [
        (if (jtarget /= flipH j0) && (ktarget == flipH k0) then 1/4 else 0) +
        (if (jtarget /= flipH j0) && (ktarget == flipT k0) then 1/4 else 0) +
        (if (jtarget /= flipT j0) && (ktarget == flipH k0) then 1/4 else 0) +
        (if (jtarget /= flipT j0) && (ktarget == flipT k0) then 1/4 else 0)
        ]

diagonalize :: [[Rational]] -> [[Rational]]
diagonalize [] = []
diagonalize [a] = [a]
diagonalize ((a@(a1:as)):rest)
  | a1 == 0 = diagonalize (rest ++ [a])
  | otherwise = a : diagonalize (map f rest)
  where f (b1:bs) = zipWith (-) bs (map (* (b1/a1)) as)

backsubst :: [[Rational]] -> [Rational]
backsubst [] = []
backsubst ([a1,a2]:rest) = (-a1/a2) : backsubst (map subst rest)
  where subst (b1:b2:bs) = (b1-b2*a1/a2):bs

p :: State -> State -> Rational
p jtarget ptarget = sum vec/64 + 7/64
  where vec = backsubst $ reverse $ map reverse $ diagonalize $ matrix jtarget ptarget

main :: IO ()
main = do
    print $ p TTT TTH
    mapM_ print [let prob = sum [p j k | j <- states, j /= k]/7 in (fromRational prob,prob,k) | k <- states]
