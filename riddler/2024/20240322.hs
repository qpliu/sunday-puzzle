import Data.List(sort)

data Match = Match Integer Integer Integer [Integer] deriving Show

finals :: [Match]
finals = [Match 1 1 2 []]

previousMatches :: Match -> [Match]
previousMatches (Match round seed1 seed2 schedule) =
    [Match (round+1) seed1 seed4 (seed2:schedule),
     Match (round+1) seed2 seed3 (seed1:schedule)]
  where
    seed4 = 2^(round+1)+1-seed1
    seed3 = 2^(round+1)+1-seed2

previousRound :: [Match] -> [Match]
previousRound = concatMap previousMatches

schedules :: Match -> [([Integer],Integer)]
schedules (Match _ seed1 seed2 schedule) =
    [(seed1:schedule,seed2),(seed2:schedule,seed1)]

getStrength :: ([Integer],Integer) -> (Integer,Integer)
getStrength (schedule,seed) = (product schedule,seed)

getToughest :: [Match] -> [Integer]
getToughest = map snd . take 2 . sort . map getStrength . concatMap schedules

toughest :: Int -> [Integer]
toughest n = getToughest $ head $ drop (n-1) $ iterate previousRound finals

main :: IO ()
main = mapM_ print [(toughest n,2^n*2/3,2^n*5/6) | n <- [1..20]]
