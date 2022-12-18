import Data.List(sort)
import Data.Map(Map,adjust,alter,empty,fromList,insert,mapKeys,size,toList)
import qualified Data.Map

-- n = number of people
-- result maps distinct collections of names to its probability
-- distinct collection of names is [count of name 1,count of name 2,...]
--   canonicalized so that
--   count of name 1 >= count of name 2 >= count of name 3...
-- the probabilities should add up to 1
hat :: Integer -> Map [Integer] Rational
hat n
  | n < 2 = error "not enough names"
  | otherwise = addName 1 (fromList [([1,0],n-1)])
  where
    addName :: Integer -> Map [Integer] Integer -> Map [Integer] Rational
    addName i names
      | i >= n = canonical names
      | otherwise = addName (i+1) $ foldl collect empty $ concatMap (addNameI n i) $ toList names
      where
        canonical :: Map [Integer] Integer ->  Map [Integer] Rational
        canonical m = Data.Map.map ((/ fromIntegral ((n-1)^n)) . fromIntegral) $ foldl collect empty $ map (\ (k,v) -> (reverse $ drop 1 $ sort k,v)) $ toList m
        collect :: Map [Integer] Integer -> ([Integer],Integer) -> Map [Integer] Integer
        collect m (k,count) = alter (Just . maybe count (+count)) k m

    addNameI :: Integer -> Integer -> ([Integer],Integer) -> [([Integer],Integer)]
    addNameI n i (counts,coefficient) = (1:counts,coefficient*(n-i-1)):[(0:sort (addPreviousName j counts),coefficient) | j <- [1..i]]

    addPreviousName :: Integer -> [Integer] -> [Integer]
    addPreviousName _ [] = error "this cannot happen"
    addPreviousName name (count:counts)
      | name == 0 = count+1:counts
      | otherwise = count:addPreviousName (name-1) counts

-- names = distinct collection of names, a key in the result of hat
-- result is a [probability of no pairs,probability of 1 pair,...]
-- the probabilities should add up to 1
pairs :: [Integer] -> [Rational]
pairs names = map ((/ fromIntegral (product [1..n])) . fromIntegral . snd) $ toList $ foldl collect empty $ gather 0 (fromList (zip [0..] names)) empty 0 1
  where
    n = sum names
    collect m (pairs,count) = alter (Just . maybe count (+count)) pairs m
    -- i is current person
    -- hat is what's in the hat from which the current person draws
    -- pending is what previous persons have drawn that could become a pair
    -- pairs is the number of pairs formed so far
    -- count is the number of possibilities represented by previous draws
    gather :: Integer -> Map Integer Integer -> Map Integer Integer -> Int -> Integer -> [(Int,Integer)]
    gather i hat pending pairs count
      | i >= n = if sum hat /= 0 then error "name left in hat" else [(pairs,count)]
      | otherwise = concatMap enumerate (toList hat)
      where
        enumerate (j,jcount)
          | jcount <= 0 = []
          | Data.Map.lookup j pending == Just i = gather (i+1) (insert j (jcount-1) hat) pending (pairs+1) (count*jcount)
          | j <= i = gather (i+1) (insert j (jcount-1) hat) pending pairs (count*jcount)
          | otherwise = gather (i+1) (insert j (jcount-1) hat) (insert i j pending) pairs (count*jcount)

-- n = number of people in round
-- result is [probability of n people,probability of n-2 people,...]
--   for next round
coefficients :: Integer -> [Rational]
coefficients n = addUp $ map calculate $ toList $ hat n
  where
    calculate :: ([Integer],Rational) -> [Rational]
    calculate (hatNames,hatProb) = take (1 + fromIntegral (n `div` 2)) $ map (hatProb*) (pairs hatNames) ++ repeat 0
    addUp :: [[Rational]] -> [Rational]
    addUp ([]:_) = []
    addUp m = sum (map head m) : addUp (map tail m)

expectedRounds :: [(Double,Rational)]
expectedRounds = er 2 []
  where
    er n rs = (fromRational r,r) : er (n+2) (r:rs)
      where
        (c0:cs) = coefficients n
        r = (1 + sum (zipWith (*) cs rs))/(1-c0)

main :: IO ()
main = mapM_ print $ zip [1..10] expectedRounds
