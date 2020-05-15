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

is6 :: Bool -> Tile -> Bool
is6 rand T06 = rand
is6 rand T16 = rand
is6 rand T26 = rand
is6 rand T36 = rand
is6 rand T46 = rand
is6 rand T56 = rand
is6 _ T66 = True
is6 _ _ = False

is1 :: Bool -> Tile -> Bool
is1 rand T01 = rand
is1 _ T11 = True
is1 rand T12 = rand
is1 rand T13 = rand
is1 rand T14 = rand
is1 rand T15 = rand
is1 rand T16 = rand
is1 _ _ = False

isDouble :: Tile -> Bool
isDouble T00 = True
isDouble T11 = True
isDouble T22 = True
isDouble T33 = True
isDouble T44 = True
isDouble T55 = True
isDouble T66 = True
isDouble _ = False

draw :: StdGen -> (Tile,Bool,StdGen)
draw rng = (tiles !! n, side, nextRng2)
  where
    tiles = [minBound..maxBound]
    (n,nextRng) = randomR (0,length tiles - 1) rng
    (side,nextRng2) = randomR (False,True) nextRng

run :: (Tile -> Bool) -> (Bool -> Tile -> Bool) -> Integer -> IO (Rational,Double)
run criterion postFilter count = newStdGen >>= r 0 0
  where
    r nCriterion n rng
        | n >= count = return (odds,fromRational odds)
        | postFilter side tile = r (nCriterion + if criterion tile then 1 else 0) (n+1) nextRng
        | otherwise = r nCriterion n nextRng
      where
        (tile,side,nextRng) = draw rng
        odds = nCriterion % n

main :: IO ()
main = do
  let n = 100000
  odds <- run isDouble (const (const True)) n
  putStrLn ("Odds of doubles with " ++ show n ++ " draws: " ++ show odds)
  odds <- run isDouble is6 n
  putStrLn ("Odds of doubles with " ++ show n ++ " draws where one side is 6: " ++ show odds)
  odds <- run isDouble is1 n
  putStrLn ("Odds of doubles with " ++ show n ++ " draws where one side is 1: " ++ show odds)
