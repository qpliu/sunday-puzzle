import Data.Map(Map,fromList,(!))

choose :: Integer -> Integer -> Rational
choose n k = fromIntegral (product [k+1..n] `div` product [1..n-k])

divs :: Integer
divs = 6

m :: Integer
m = 5

f :: Map (Integer,Integer) Rational
f = fromList [((n,p),ffunc n p) | n <- [1..divs], p <- [0..(n-1)*m]]
  where
    ffunc n p = counts!(m,p)/choose (n*m) m
      where
        counts = fromList [((divTeams,nonDivTeams),cnts divTeams nonDivTeams) | divTeams <- [0..m], nonDivTeams <- [0..p]]
        cnts divTeams nonDivTeams
          | nonDivTeams <= 0 = 1
          | divTeams <= 0 = 0
          | otherwise = counts!(divTeams-1,nonDivTeams) + counts!(divTeams,nonDivTeams-1)

w1 :: Map (Integer,Integer,Integer) Rational
w1 = fromList [((n,p,q),w1func n p q) | n <- [1..divs], p <- [0..n*m-1], q <- [0..m]]
  where
    w1func n p q = counts!(0,0)/choose (n*m) m
      where
        counts = fromList [((divTeams,nonDivTeams),cnts divTeams nonDivTeams) | divTeams <- [0..m], nonDivTeams <- [0..(n-1)*m]]
        cnts divTeams nonDivTeams
          | divTeams == m-q && divTeams + nonDivTeams == p = 1
          | divTeams > m-q = 0
          | divTeams + nonDivTeams > p = 0
          | nonDivTeams >= (n-1)*m = 0
          | divTeams > m-1 = 0
          | divTeams + nonDivTeams < p = counts!(divTeams+1,nonDivTeams) + counts!(divTeams,nonDivTeams+1)
          | otherwise = 0

w :: Map (Integer,Integer) Rational
w = fromList [((n,p),wfunc n p) | n <- [1..divs], p <- [0..m*n-1]]
  where
    wfunc n p
      | n <= 1 = 0
      | otherwise = w1!(n,p,m) + sum [w1!(n,p,q)*w!(n-1,p-m+q) | q <- [0..m-1], p-m+q >= 0, p-m+q < m*(n-1)]

s1 :: Map Integer Rational
s1 = fromList [(n,s1func n) | n <- [1..divs]]
  where
    s1func 1 = 0
    s1func n = sum [f!(n,p)*w!(n-1,p) | p <- [0..(n-1)*m-1]]

s :: Integer -> Rational
s n
  | n <= 1 = 0
  | otherwise = 2*s1!n + (1 - 2*s1!n)*s (n-1)

bigp :: Rational -> Integer -> Rational
bigp p n
  | n <= 1 = 0
  | otherwise = q (n-1)
  where
    q m
      | m <= 0 = bigp p (n-1)
      | otherwise = p + (1-p)*q (m-1)
