import Data.Ratio((%))

expected :: Rational -> Integer -> Integer -> Rational
expected chance guessNumber guessOptions
  | guessOptions == 0 = 0
  | guessOptions `mod` 2 == 1 =
      chance * (guessNumber % guessOptions)
          + expected (chance * ((guessOptions - 1) % guessOptions))
                     (guessNumber + 1) (guessOptions `div` 2)
  | otherwise =
      chance * (guessNumber % guessOptions)
          + expected (chance * ((guessOptions `div` 2) % guessOptions))
                     (guessNumber + 1) (guessOptions `div` 2)
          + expected (chance * ((guessOptions `div` 2 - 1) % guessOptions))
                     (guessNumber + 1) (guessOptions `div` 2 - 1)

main :: IO ()
main = do
    print answer
    print (fromRational answer :: Double)
  where
    answer = expected 1 1 267751
