type Game = (Int,[(Int,Int,Int)])

parse :: String -> [Game]
parse input = pgame (words input) []
  where
    pgame [] games = games
    pgame ("Game":gameID:rest) games = psets rest (read (init gameID)) (0,0,0) [] games
    psets [] gameID set sets games = (gameID,set:sets):games
    psets (n:"red":rest)    gameID (_,g,b) sets games = pgame rest ((gameID,(read n,g,b):sets):games)
    psets (n:"red,":rest)   gameID (_,g,b) sets games = psets rest gameID (read n,g,b) sets games
    psets (n:"red;":rest)   gameID (_,g,b) sets games = psets rest gameID (0,0,0) ((read n,g,b):sets) games
    psets (n:"blue":rest)   gameID (r,g,_) sets games = pgame rest ((gameID,(r,g,read n):sets):games)
    psets (n:"blue,":rest)  gameID (r,g,_) sets games = psets rest gameID (r,g,read n) sets games
    psets (n:"blue;":rest)  gameID (r,g,_) sets games = psets rest gameID (0,0,0) ((r,g,read n):sets) games
    psets (n:"green":rest)  gameID (r,_,b) sets games = pgame rest ((gameID,(r,read n,b):sets):games)
    psets (n:"green,":rest) gameID (r,_,b) sets games = psets rest gameID (r,read n,b) sets games
    psets (n:"green;":rest) gameID (r,_,b) sets games = psets rest gameID (0,0,0) ((r,read n,b):sets) games

possibleID :: Game -> Int
possibleID (gameID,sets) = p sets
  where
    p [] = gameID
    p ((r,g,b):rest)
      | r > 12 || g > 13 || b > 14 = 0
      | otherwise = p rest

result :: String -> Int
result = sum . map possibleID . parse

testData :: String
testData = unlines [
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    ]

test :: ()
test
  | result testData /= 8 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/02.txt"

possible2 :: Game -> (Int,Int,Int)
possible2 (_,sets) = foldl collect (0,0,0) sets
  where
    collect (r1,g1,b1) (r2,g2,b2) = (max r1 r2,max g1 g2,max b1 b2)

power2 :: (Int,Int,Int) -> Int
power2 (r,g,b) = r*g*b

result2 :: String -> Int
result2 = sum . map (power2 . possible2) . parse

test2 :: ()
test2
  | result2 testData /= 2286 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/02.txt"
