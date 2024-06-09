import Data.Map(Map,fromList,toList,(!))

choose :: Integer -> Integer -> Integer
choose n k = fromIntegral (product [k+1..n]) `div` fromIntegral (product [1..n-k])

scores :: Integer -> [(Integer,Integer)]
scores npossessions = [(us,them) | us <- [0..npossessions], them <- [0..npossessions - us]]

theirChancesOfWinning :: Integer -> (Integer,Integer) -> Rational
theirChancesOfWinning npossessions (us,them) = fromIntegral (sum [choose n i| i <- [npossessions `div` 2 + 1 - them .. n]])/2^n
  where
    n = npossessions - us - them

nMatrix :: Integer -> Rational -> Map (Integer,Integer) Integer
nMatrix npossessions threshold = table
  where
    table = fromList [(score,nWays score) | score <- scores npossessions]
    nWays (x,y)
      | x + y < npossessions && theirChancesOfWinning npossessions (x,y) >= threshold = choose (x+y) x
      | x > 0 && y > 0 = table!(x-1,y) + table!(x,y-1)
      | x > 0 = table!(x-1,y)
      | y > 0 = table!(x,y-1)
      | otherwise = 0

ourProbability :: Integer -> Rational -> (Integer,Integer,Rational)
ourProbability npossessions threshold = (wins,games,fromIntegral wins/fromIntegral games)
  where
    n = nMatrix npossessions threshold
    games = sum [n | ((x,y),n) <- toList n, x+y == npossessions]
    wins = sum [n | ((x,y),n) <- toList n, x+y == npossessions, x>y]

ftMatrix :: Integer -> Rational -> Map (Integer,Integer) Integer
ftMatrix npossessions threshold = table
  where
    table = fromList [(score,nWays score) | score <- scores npossessions]
    nWays (x,y)
      | x <= 0 || y <= 0 = 1
      | chances >= threshold || chances <= 1-threshold = 0
      | otherwise = table!(x-1,y) + table!(x,y-1)
      where chances = theirChancesOfWinning npossessions (x,y)
