import Data.List(nub)

type Points = (Int,Int,Int,Int)

incA :: Points -> Points
incA (a,b,c,d) = (a+1,b,c,d)

incB :: Points -> Points
incB (a,b,c,d) = (a,b+1,c,d)

incC :: Points -> Points
incC (a,b,c,d) = (a,b,c+1,d)

incD :: Points -> Points
incD (a,b,c,d) = (a,b,c,d+1)

game :: (Points -> Points) -> (Points -> Points) -> [Points] -> [Points]
game team1 team2 points = concatMap g points
  where g points = [team1 (team1 (team1 points)), team2 (team2 (team2 points)), team1 (team2 points)]

keep :: Points -> Bool
keep (a,b,c,d) = a > b && b > c && c > d

groupPlay :: [Points]
groupPlay = (game incA incB . game incC incD . game incA incC .
             game incB incD . game incA incD . game incB incC) [(0,0,0,0)]

main :: IO ()
main = print $ nub $ filter keep groupPlay
