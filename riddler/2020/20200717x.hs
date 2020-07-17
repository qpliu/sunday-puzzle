import Data.Ratio((%))

ba :: Rational
ba = 35 % 100

c :: Integer -> Integer -> Rational
c n k = product [k+1 .. n] % product [1 .. n-k]

exactly :: Integer -> Integer -> Rational
exactly hits atbats = c atbats hits * product (take (fromIntegral hits) (repeat ba)) * product (take (fromIntegral (atbats - hits)) (repeat ((1%1) - ba)))

atleast :: Integer -> Integer -> Rational
atleast hits atbats = sum [exactly h atbats | h <- [hits..atbats]]

h :: Rational
h = product (take 4 (repeat (1%1 - ba)))

s :: Int -> Int -> Rational
s n m
  | m < 0 = 0%1
  | otherwise = hn + (1%1 - hn)*s n (m-1)
  where hn = product (take n (repeat h))

main :: IO ()
main = do
    let c60 = atleast 96 240
    print c60
    print (fromRational c60 :: Double)
    -- let c162 = atleast 260 648
    -- print c162
    -- print (fromRational c162 :: Double)
    let s60 = s 56 60
    print s60
    print (fromRational s60 :: Double)
    let s162 = s 56 162
    print s162
    print (fromRational s162 :: Double)
