{-
--- Day 22: Crab Combat ---

It only takes a few hours of sailing the ocean on a raft for boredom to sink
in. Fortunately, you brought a small deck of space cards! You'd like to play a
game of Combat, and there's even an opponent available: a small crab that
climbed aboard your raft before you left.

Fortunately, it doesn't take long to teach the crab the rules.

Before the game starts, split the cards so each player has their own deck (your
puzzle input). Then, the game consists of a series of rounds: both players draw
their top card, and the player with the higher-valued card wins the round. The
winner keeps both cards, placing them on the bottom of their own deck so that
the winner's card is above the other card. If this causes a player to have all
of the cards, they win, and the game ends.

For example, consider the following starting decks:

| Player 1:
| 9
| 2
| 6
| 3
| 1
| 
| Player 2:
| 5
| 8
| 4
| 7
| 10

This arrangement means that player 1's deck contains 5 cards, with 9 on top and
1 on the bottom; player 2's deck also contains 5 cards, with 5 on top and 10 on
the bottom.

The first round begins with both players drawing the top card of their decks: 9
and 5. Player 1 has the higher card, so both cards move to the bottom of player
1's deck such that 9 is above 5. In total, it takes 29 rounds before a player
has all of the cards:

| -- Round 1 --
| Player 1's deck: 9, 2, 6, 3, 1
| Player 2's deck: 5, 8, 4, 7, 10
| Player 1 plays: 9
| Player 2 plays: 5
| Player 1 wins the round!
| 
| -- Round 2 --
| Player 1's deck: 2, 6, 3, 1, 9, 5
| Player 2's deck: 8, 4, 7, 10
| Player 1 plays: 2
| Player 2 plays: 8
| Player 2 wins the round!
| 
| -- Round 3 --
| Player 1's deck: 6, 3, 1, 9, 5
| Player 2's deck: 4, 7, 10, 8, 2
| Player 1 plays: 6
| Player 2 plays: 4
| Player 1 wins the round!
| 
| -- Round 4 --
| Player 1's deck: 3, 1, 9, 5, 6, 4
| Player 2's deck: 7, 10, 8, 2
| Player 1 plays: 3
| Player 2 plays: 7
| Player 2 wins the round!
| 
| -- Round 5 --
| Player 1's deck: 1, 9, 5, 6, 4
| Player 2's deck: 10, 8, 2, 7, 3
| Player 1 plays: 1
| Player 2 plays: 10
| Player 2 wins the round!
| 
| ...several more rounds pass...
| 
| -- Round 27 --
| Player 1's deck: 5, 4, 1
| Player 2's deck: 8, 9, 7, 3, 2, 10, 6
| Player 1 plays: 5
| Player 2 plays: 8
| Player 2 wins the round!
| 
| -- Round 28 --
| Player 1's deck: 4, 1
| Player 2's deck: 9, 7, 3, 2, 10, 6, 8, 5
| Player 1 plays: 4
| Player 2 plays: 9
| Player 2 wins the round!
| 
| -- Round 29 --
| Player 1's deck: 1
| Player 2's deck: 7, 3, 2, 10, 6, 8, 5, 9, 4
| Player 1 plays: 1
| Player 2 plays: 7
| Player 2 wins the round!
| 
| 
| == Post-game results ==
| Player 1's deck: 
| Player 2's deck: 3, 2, 10, 6, 8, 5, 9, 4, 7, 1

Once the game ends, you can calculate the winning player's score. The bottom
card in their deck is worth the value of the card multiplied by 1, the
second-from-the-bottom card is worth the value of the card multiplied by 2, and
so on. With 10 cards, the top card is worth the value on the card multiplied by
10. In this example, the winning player's score is:

|    3 * 10
| +  2 *  9
| + 10 *  8
| +  6 *  7
| +  8 *  6
| +  5 *  5
| +  9 *  4
| +  4 *  3
| +  7 *  2
| +  1 *  1
| = 306

So, once the game ends, the winning player's score is 306.

Play the small crab in a game of Combat using the two decks you just dealt.
What is the winning player's score?
-}

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
