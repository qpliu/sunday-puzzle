import Data.Ratio((%))

data Chocolate = Milk | Dark deriving Eq

chanceThatLastIsMilk :: Integer -> Integer -> Maybe Chocolate -> Rational
chanceThatLastIsMilk numberOfMilk numberOfDark lastChocolate
  | numberOfMilk == 0 = 0
  | numberOfDark == 0 = 1
  | lastChocolate == Nothing =
      (numberOfMilk%total)*chanceThatLastIsMilk (numberOfMilk-1) numberOfDark (Just Milk) +
      (numberOfDark%total)*chanceThatLastIsMilk numberOfMilk (numberOfDark-1) (Just Dark)
  | lastChocolate == Just Milk =
      (numberOfMilk%total)*chanceThatLastIsMilk (numberOfMilk-1) numberOfDark (Just Milk) +
      (numberOfDark%total)*chanceThatLastIsMilk numberOfMilk numberOfDark Nothing
  | otherwise =
      (numberOfMilk%total)*chanceThatLastIsMilk numberOfMilk numberOfDark Nothing +
      (numberOfDark%total)*chanceThatLastIsMilk numberOfMilk (numberOfDark-1) (Just Dark)
  where total = numberOfMilk+numberOfDark

main :: IO ()
main = print (chanceThatLastIsMilk 2 8 Nothing)
