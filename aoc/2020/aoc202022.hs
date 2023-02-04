import Data.Set(Set,empty,insert,member)

parse :: String -> (([Int],[Int]),([Int],[Int]))
parse str = ((map read p1,[]),(map read (drop 2 p2),[]))
  where
    (p1,p2) = span (/= "Player") (drop 2 (words str))

playRound :: (([Int],[Int]),([Int],[Int])) -> (([Int],[Int]),([Int],[Int]))
playRound hands@(([],[]),_) = hands
playRound hands@(_,([],[])) = hands
playRound (([],wins),p2) = playRound ((reverse wins,[]),p2)
playRound (p1,([],wins)) = playRound (p1,(reverse wins,[]))
playRound ((p1:p1s,wins1),(p2:p2s,wins2))
  | p1 > p2 = ((p1s,p2:p1:wins1),(p2s,wins2))
  | otherwise = ((p1s,wins1),(p2s,p1:p2:wins2))

notEnded :: (([Int],[Int]),([Int],[Int])) -> Bool
notEnded (([],[]),_) = False
notEnded (_,([],[])) = False
notEnded _ = True

scores :: (([Int],[Int]),([Int],[Int])) -> (Int,Int)
scores (p1,p2) = (score p1,score p2)

score :: ([Int],[Int]) -> Int
score (top,bottom) = sum $ zipWith (*) [1..] $ bottom ++ reverse top

play :: (([Int],[Int]),([Int],[Int])) -> (Int,Int)
play hands = scores $ head $ dropWhile notEnded $ iterate playRound hands

testData :: String
testData = "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10\n"

test :: ()
test
  | (play . parse) testData /= (0,306) = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (uncurry (+) . play . parse) $ readFile "input/22.txt"

toPart2 :: (([Int],[Int]),([Int],[Int])) -> ([Int],[Int])
toPart2 ((t1,b1),(t2,b2)) = (t1 ++ reverse b1,t2 ++ reverse b2)

scores2 :: ([Int],[Int]) -> (Int,Int)
scores2 (p1,p2) = (score2 p1,score2 p2)

score2 :: [Int] -> Int
score2 = sum . zipWith (*) [1..] . reverse

play2 :: Set ([Int],[Int]) -> ([Int],[Int]) -> ([Int],[Int])
play2 states state@(p1,p2)
  | member state states = (p1++p2,[])
  | null p1 || null p2 = state
  | c1 > length p1 - 1 || c2 > length p2 - 1 =
      if c1 > c2 then p1wins else p2wins
  | (null . fst . play2 empty) (take c1 (tail p1),take c2 (tail p2)) = p2wins
  | otherwise = p1wins
  where
    c1 = head p1
    c2 = head p2
    p1wins = play2 (insert state states) (tail p1 ++ [c1,c2],tail p2)
    p2wins = play2 (insert state states) (tail p1,tail p2 ++ [c2,c1])

test2 :: ()
test2
  | (scores2 . play2 empty . toPart2 . parse) testData /= (0,291) = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (uncurry max . scores2 . play2 empty . toPart2 . parse) $ readFile "input/22.txt"
