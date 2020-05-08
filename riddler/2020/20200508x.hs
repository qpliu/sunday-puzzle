import Data.Ratio((%))
import System.Random(StdGen,newStdGen,randomR)

data Tile = T00
          | T01 | T11
          | T02 | T12 | T22
          | T03 | T13 | T23 | T33
          | T04 | T14 | T24 | T34 | T44
          | T05 | T15 | T25 | T35 | T45 | T55
          | T06 | T16 | T26 | T36 | T46 | T56 | T66
    deriving (Bounded,Enum)

is6 :: Tile -> Bool
is6 T06 = True
is6 T16 = True
is6 T26 = True
is6 T36 = True
is6 T46 = True
is6 T56 = True
is6 T66 = True
is6 _ = False

is1 :: Tile -> Bool
is1 T01 = True
is1 T11 = True
is1 T12 = True
is1 T13 = True
is1 T14 = True
is1 T15 = True
is1 T16 = True
is1 _ = False

isDouble :: Tile -> Bool
isDouble T00 = True
isDouble T11 = True
isDouble T22 = True
isDouble T33 = True
isDouble T44 = True
isDouble T55 = True
isDouble T66 = True
isDouble _ = False

draw :: StdGen -> (Tile,StdGen)
draw rng = (tiles !! n, nextRng)
  where
    tiles = [minBound..maxBound]
    (n,nextRng) = randomR (0,length tiles - 1) rng

run :: (Tile -> Bool) -> (Tile -> Bool) -> Integer -> IO (Rational,Double)
run criterion postFilter count = newStdGen >>= r 0 0
  where
    r nCriterion n rng
        | n >= count = return (odds,fromRational odds)
        | postFilter tile = r (nCriterion + if criterion tile then 1 else 0) (n+1) nextRng
        | otherwise = r nCriterion n nextRng
      where
        (tile,nextRng) = draw rng
        odds = nCriterion % n

main :: IO ()
main = do
  let n = 100000
  odds <- run isDouble (const True) n
  putStrLn ("Odds of doubles with " ++ show n ++ " draws: " ++ show odds)
  odds <- run isDouble is6 n
  putStrLn ("Odds of doubles with " ++ show n ++ " draws where one side is 6: " ++ show odds)
  odds <- run isDouble is1 n
  putStrLn ("Odds of doubles with " ++ show n ++ " draws where one side is 1: " ++ show odds)
