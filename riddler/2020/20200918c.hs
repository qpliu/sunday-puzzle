import Data.Ratio((%))

expected :: Integer -> Integer -> Rational
expected guessNumber guessOptions
  | guessOptions == 0 = 0
  | guessOptions `mod` 2 == 1 =
      guessNumber % guessOptions
          + ((guessOptions - 1) % guessOptions)
              * expected (guessNumber + 1) (guessOptions `div` 2)
  | otherwise =
      guessNumber % guessOptions
          + ((guessOptions `div` 2) % guessOptions)
              * expected (guessNumber + 1) (guessOptions `div` 2)
          + ((guessOptions `div` 2 - 1) % guessOptions)
              * expected (guessNumber + 1) (guessOptions `div` 2 - 1)

main :: IO ()
main = do
    print answer
    print (fromRational answer :: Double)
  where
    answer = expected 1 267751
