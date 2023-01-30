{-
--- Day 21: Dirac Dice ---

There's not much to do as you slowly descend to the bottom of the ocean. The
submarine computer challenges you to a nice game of Dirac Dice.

This game consists of a single die, two pawns, and a game board with a circular
track containing ten spaces marked 1 through 10 clockwise. Each player's
starting space is chosen randomly (your puzzle input). Player 1 goes first.

Players take turns moving. On each player's turn, the player rolls the die
three times and adds up the results. Then, the player moves their pawn that
many times forward around the track (that is, moving clockwise on spaces in
order of increasing value, wrapping back around to 1 after 10). So, if a player
is on space 7 and they roll 2, 2, and 1, they would move forward 5 times, to
spaces 8, 9, 10, 1, and finally stopping on 2.

After each player moves, they increase their score by the value of the space
their pawn stopped on. Players' scores start at 0. So, if the first player
starts on space 7 and rolls a total of 5, they would stop on space 2 and add 2
to their score (for a total score of 2). The game immediately ends as a win for
any player whose score reaches at least 1000.

Since the first game is a practice game, the submarine opens a compartment
labeled deterministic dice and a 100-sided die falls out. This die always rolls
1 first, then 2, then 3, and so on up to 100, after which it starts over at 1
again. Play using this die.

For example, given these starting positions:

| Player 1 starting position: 4
| Player 2 starting position: 8

This is how the game would go:

 - Player 1 rolls 1+2+3 and moves to space 10 for a total score of 10.
 - Player 2 rolls 4+5+6 and moves to space 3 for a total score of 3.
 - Player 1 rolls 7+8+9 and moves to space 4 for a total score of 14.
 - Player 2 rolls 10+11+12 and moves to space 6 for a total score of 9.
 - Player 1 rolls 13+14+15 and moves to space 6 for a total score of 20.
 - Player 2 rolls 16+17+18 and moves to space 7 for a total score of 16.
 - Player 1 rolls 19+20+21 and moves to space 6 for a total score of 26.
 - Player 2 rolls 22+23+24 and moves to space 6 for a total score of 22.

...after many turns...

 - Player 2 rolls 82+83+84 and moves to space 6 for a total score of 742.
 - Player 1 rolls 85+86+87 and moves to space 4 for a total score of 990.
 - Player 2 rolls 88+89+90 and moves to space 3 for a total score of 745.
 - Player 1 rolls 91+92+93 and moves to space 10 for a final score, 1000.

Since player 1 has at least 1000 points, player 1 wins and the game ends. At
this point, the losing player had 745 points and the die had been rolled a
total of 993 times; 745 * 993 = 739785.

Play a practice game using the deterministic 100-sided die. The moment either
player wins, what do you get if you multiply the score of the losing player by
the number of times the die was rolled during the game?
-}

import Data.Map(Map,fromList,(!))

roll3x100 :: Int -> Int
roll3x100 rolls = (6 + rolls*3) `mod` 10

takeTurn :: (Int,(Int,Int)) -> (Int,(Int,Int))
takeTurn (score,(rolls,pos)) = (score+newpos,(rolls+6,newpos))
  where
    newpos = (pos - 1 + roll3x100 rolls) `mod` 10 + 1

init1000 :: (Int,Int) -> ((Int,(Int,Int)),(Int,(Int,Int)))
init1000 (pos1,pos2) = ((0,(0,pos1)),(0,(3,pos2)))

play1000 :: ((Int,(Int,Int)),(Int,(Int,Int))) ->  ((Int,(Int,Int)),(Int,(Int,Int)))
play1000 (p1,p2@(s2,_))
  | s2 >= 1000 = (p1,p2)
  | otherwise = play1000 (p2,takeTurn p1)

result1000 :: ((Int,(Int,Int)),(Int,(Int,Int))) -> Int
result1000 ((score,(rolls,_)),_) = rolls*score

test :: ()
test
  | (result1000 . play1000 . init1000) (4,8) /= 739785 = error "a"
  | otherwise = ()

part1 :: (Int,Int) -> Int
part1 = result1000 . play1000 . init1000

-- 3: (1x) 1 1 1 
-- 4: (3x) 1 1 2,1 2 1,2 1 1
-- 5: (6x) 1 1 3,1 3 1,3 1 1,1 2 2,2 1 2,2 2 1
-- 6: (7x) 1 2 3,1 3 2,2 1 3,2 3 1,3 1 2,3 2 1,2 2 2
-- 7: (6x) 3 3 1,3 1 3,1 3 3,3 2 2,2 3 2,2 2 3
-- 8: (3x) 3 3 2,3 2 3,2 3 3
-- 9: (1x) 3 3 3

outcomes :: Map (Bool,(Int,Int),(Int,Int)) (Integer,Integer)
outcomes = table
  where
    table = fromList [play (player,(pos1,score1),(pos2,score2)) | player <- [True,False], pos1 <- [1..10], score1 <- [0..20], pos2 <- [1..10], score2 <- [0..20]]

    (/*) a (x,y) = (x*a,y*a)
    (/+) (x1,y1) (x2,y2) = (x1+x2,y1+y2)
    play state@(True,(pos1,score1),p2) = (state,
           (1 /* (roll 3))
        /+ (3 /* (roll 4))
        /+ (6 /* (roll 5))
        /+ (7 /* (roll 6))
        /+ (6 /* (roll 7))
        /+ (3 /* (roll 8))
        /+ (1 /* (roll 9)))
      where
        roll n
          | newpos + score1 >= 21 = (1,0)
          | otherwise = table!(False,(newpos,score1+newpos),p2)
          where newpos = (pos1-1+n) `mod` 10 + 1
    play state@(False,p1,(pos2,score2)) = (state,
           (1 /* (roll 3))
        /+ (3 /* (roll 4))
        /+ (6 /* (roll 5))
        /+ (7 /* (roll 6))
        /+ (6 /* (roll 7))
        /+ (3 /* (roll 8))
        /+ (1 /* (roll 9)))
      where
        roll n
          | newpos + score2 >= 21 = (0,1)
          | otherwise = table!(True,p1,(newpos,score2+newpos))
          where newpos = (pos2-1+n) `mod` 10 + 1

run2 :: (Int,Int) -> (Integer,Integer)
run2 (pos1,pos2) = outcomes!(True,(pos1,0),(pos2,0))

test2 :: ()
test2
  | run2 (4,8) /= (444356092776315,341960390180808) = error "a"
  | otherwise = ()

part2 :: (Int,Int) -> Integer
part2 = uncurry max . run2
