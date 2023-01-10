{-
--- Day 9: Marble Mania ---

You talk to the Elves while you wait for your navigation system to initialize.
To pass the time, they introduce you to their favorite marble game.

The Elves play this game by taking turns arranging the marbles in a circle
according to very particular rules. The marbles are numbered starting with 0
and increasing by 1 until every marble has a number.

First, the marble numbered 0 is placed in the circle. At this point, while it
contains only a single marble, it is still a circle: the marble is both
clockwise from itself and counter-clockwise from itself. This marble is
designated the current marble.

Then, each Elf takes a turn placing the lowest-numbered remaining marble into
the circle between the marbles that are 1 and 2 marbles clockwise of the
current marble. (When the circle is large enough, this means that there is one
marble between the marble that was just placed and the current marble.) The
marble that was just placed then becomes the current marble.

However, if the marble that is about to be placed has a number which is a
multiple of 23, something entirely different happens. First, the current player
keeps the marble they would have placed, adding it to their score. In addition,
the marble 7 marbles counter-clockwise from the current marble is removed from
the circle and also added to the current player's score. The marble located
immediately clockwise of the marble that was removed becomes the new current
marble.

For example, suppose there are 9 players. After the marble with value 0 is
placed in the middle, each player (shown in square brackets) takes a turn. The
result of each of those turns would produce circles of marbles like this, where
clockwise is to the right and the resulting current marble is in parentheses:

| [-] (0)
| [1]  0 (1)
| [2]  0 (2) 1 
| [3]  0  2  1 (3)
| [4]  0 (4) 2  1  3 
| [5]  0  4  2 (5) 1  3 
| [6]  0  4  2  5  1 (6) 3 
| [7]  0  4  2  5  1  6  3 (7)
| [8]  0 (8) 4  2  5  1  6  3  7 
| [9]  0  8  4 (9) 2  5  1  6  3  7 
| [1]  0  8  4  9  2(10) 5  1  6  3  7 
| [2]  0  8  4  9  2 10  5(11) 1  6  3  7 
| [3]  0  8  4  9  2 10  5 11  1(12) 6  3  7 
| [4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7 
| [5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7 
| [6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
| [7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15 
| [8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15 
| [9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15 
| [1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15 
| [2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15 
| [3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15 
| [4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15 
| [5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15 
| [6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15 
| [7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15

The goal is to be the player with the highest score after the last marble is
used up. Assuming the example above ends after the marble numbered 25, the
winning score is 23+9=32 (because player 5 kept marble 23 and removed marble 9,
while no other player got any points in this very short example game).

Here are a few more examples:

 - 10 players; last marble is worth 1618 points: high score is 8317
 - 13 players; last marble is worth 7999 points: high score is 146373
 - 17 players; last marble is worth 1104 points: high score is 2764
 - 21 players; last marble is worth 6111 points: high score is 54718
 - 30 players; last marble is worth 5807 points: high score is 37305

What is the winning Elf's score?
-}

import Data.Array(Array,array,(!),(//))

type State = (Int,[Int],([Int],[Int])) -- next marble, circle, scores

initState :: Int -> State
initState numPlayers = (1,[0],(take numPlayers $ repeat 0,[]))

turn :: State -> State
turn state@(nextMarble,circle,(playersYetToPlay,playersPlayed))
  | null playersYetToPlay = turn (nextMarble,circle,(reverse playersPlayed,[]))
  | nextMarble `mod` 23 /= 0 = (nextMarble+1,nextMarble:circle2++circle0,(tail playersYetToPlay,head playersYetToPlay:playersPlayed))
  | otherwise = (nextMarble+1,circle6++circle8,(tail playersYetToPlay,(head playersYetToPlay + m7 + nextMarble):playersPlayed))
  where
    (circle0,circle2) = splitAt 2 circle
    (circle8,m7:circle6) = splitAt (length circle - 7) circle

scores :: State -> [Int]
scores (_,_,(playersYetToPlay,playersPlayed)) = reverse playersPlayed ++ playersYetToPlay

play :: Int -> Int -> [Int]
play numPlayers lastMarble = scores $ head $ drop lastMarble $ iterate turn (initState numPlayers)

test :: ()
test
  | maximum (play 9 25) /= 32 = error "a"
  | maximum (play 10 1618) /= 8317 = error "b"
  | maximum (play 13 7999) /= 146373 = error "c"
  | maximum (play 17 1104) /= 2764 = error "d"
  | maximum (play 21 6111) /= 54718 = error "e"
  | maximum (play 30 5807) /= 37305 = error "f"
  | otherwise = ()

-- this is super slow
part1 :: Int -> Int -> Int
part1 numPlayers lastMarble = maximum $ play numPlayers lastMarble

-- much faster, but still scales just as poorly
scoring2 :: [(Int,Int)]
scoring2 = (23,9):s 46 [17,42,4,43,18,44,19,45,2,24,20,25,10,26,21,27,5,28,22,29,11,30,1,31,12,32,6,33,13,34,3,35,14,36,7,37,15,38,0,39,16,40,8,41]
  where
    s m (m1:m2:m3:m4:m5:m6:m7:m8:m9:m10:m11:m12:m13:m14:m15:m16:m17:m18:m19:m20:m21:m22:m23:m24:rest) = (m,m1):s (m+23) (m21:m+19:m22:m+20:m23:m+21:m24:m+22:rest ++ [m2,m3,m+1,m4,m+2,m5,m+3,m6,m+4,m7,m+5,m8,m+6,m9,m+7,m10,m+8,m11,m+9,m12,m+10,m13,m+11,m14,m+12,m15,m+13,m16,m+14,m17,m+15,m18,m+16,m19,m+17,m20,m+18])

scores2 :: Int -> [(Int,(Int,Array Int Int))]
scores2 nplayers = s scoring2 $ array (0,nplayers-1) $ zip [0..nplayers-1] $ repeat 0
  where
    s ((n,m):rest) players = (n,(m,newplayers)) : s rest newplayers
      where p = n `mod` nplayers
            newplayers = players // [(p,players!p + n + m)]

part2 :: Int -> Int -> Int
part2 nplayers lastMarble = maximum $ snd $ snd $ head $ dropWhile ((< (lastMarble-22)) . fst) $ scores2 nplayers
