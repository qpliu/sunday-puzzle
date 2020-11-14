import Data.Ratio((%))

choose :: Integer -> Integer -> Rational
choose n k = product [k+1..n] % product [1..n-k]

chanceOfWinning :: Integer -> Integer -> Rational
chanceOfWinning remainingFlips flipsNeededToWin =
    sum [choose remainingFlips i | i <- [flipsNeededToWin .. remainingFlips]] * (1 % (2^remainingFlips))

needToWinAtMostFor99 :: Integer -> Integer
needToWinAtMostFor99 remainingFlips = maximum [i | i <- [0..remainingFlips], chanceOfWinning remainingFlips i >= 99 % 100]

incrementalChancesOfEpicChoke :: [((Integer,(Rational,Integer,Integer,Integer)),Rational)]
incrementalChancesOfEpicChoke = chances 101 (0,101,31)
  where
    chances remainingFlips (previousChancesOf99,previousRemainingFlips,previousMaxWinsNeeded)
      | remainingFlips <= 0 = []
      | maxWinsNeeded >= previousMaxWinsNeeded = ((102-remainingFlips,(0,0,maxWinsNeeded,previousMaxWinsNeeded)),0) : chances (remainingFlips-1) (previousChancesOf99,previousRemainingFlips,previousMaxWinsNeeded)
      | flipsDone < previousFlipsNeededToHaveWon = ((102-remainingFlips,(0,0,maxWinsNeeded,previousMaxWinsNeeded)),0) : chances (remainingFlips-1) (previousChancesOf99,previousRemainingFlips,previousMaxWinsNeeded)
      | maxWinsNeeded /= previousMaxWinsNeeded - 1 = error ("Unexpected jump in wins needed:" ++ show (maxWinsNeeded,previousMaxWinsNeeded))
      | otherwise = ((102-remainingFlips,(chanceOfHaving99,remainingFlips,maxWinsNeeded,previousMaxWinsNeeded)),(chanceOfHaving99 - previousChancesOf99 * incrementalChanceOfAlready99) * (1 - chanceOfWinning remainingFlips maxWinsNeeded)) : chances (remainingFlips-1) (chanceOfHaving99,remainingFlips,maxWinsNeeded)
      where
        flipsDone = 101 - remainingFlips
        maxWinsNeeded = needToWinAtMostFor99 remainingFlips
        previousFlipsNeededToHaveWon = 51 - maxWinsNeeded
        chanceOfHaving99 = choose (flipsDone-1) previousFlipsNeededToHaveWon * (1 % (2^flipsDone))
        incrementalChanceOfAlready99 = choose (previousRemainingFlips - remainingFlips) (maxWinsNeeded - previousMaxWinsNeeded) * (1 % (2^(previousRemainingFlips - remainingFlips)))

main :: IO ()
main = do
    mapM_ print [(102-i,51 - needToWinAtMostFor99 i) | i <- [1..101]]
    mapM_ print incrementalChancesOfEpicChoke
    let answer = sum (map snd incrementalChancesOfEpicChoke)
    print answer
    print (fromRational answer :: Double)
