{-
--- Day 22: Slam Shuffle ---

There isn't much to do while you wait for the droids to repair your ship. At
least you're drifting in the right direction. You decide to practice a new card
shuffle you've been working on.

Digging through the ship's storage, you find a deck of space cards! Just like
any deck of space cards, there are 10007 cards in the deck numbered 0 through
10006. The deck must be new - they're still in factory order, with 0 on the
top, then 1, then 2, and so on, all the way through to 10006 on the bottom.

You've been practicing three different techniques that you use while shuffling.
Suppose you have a deck of only 10 cards (numbered 0 through 9):

To deal into new stack, create a new stack of cards by dealing the top card of
the deck onto the top of the new stack repeatedly until you run out of cards:

| Top          Bottom
| 0 1 2 3 4 5 6 7 8 9   Your deck
|                       New stack
| 
|   1 2 3 4 5 6 7 8 9   Your deck
|                   0   New stack
| 
|     2 3 4 5 6 7 8 9   Your deck
|                 1 0   New stack
| 
|       3 4 5 6 7 8 9   Your deck
|               2 1 0   New stack
| 
| Several steps later...
| 
|                   9   Your deck
|   8 7 6 5 4 3 2 1 0   New stack
| 
|                       Your deck
| 9 8 7 6 5 4 3 2 1 0   New stack

Finally, pick up the new stack you've just created and use it as the deck for
the next technique.

To cut N cards, take the top N cards off the top of the deck and move them as a
single unit to the bottom of the deck, retaining their order. For example, to
cut 3:

| Top          Bottom
| 0 1 2 3 4 5 6 7 8 9   Your deck
| 
|       3 4 5 6 7 8 9   Your deck
| 0 1 2                 Cut cards
| 
| 3 4 5 6 7 8 9         Your deck
|               0 1 2   Cut cards
| 
| 3 4 5 6 7 8 9 0 1 2   Your deck

You've also been getting pretty good at a version of this technique where N is
negative! In that case, cut (the absolute value of) N cards from the bottom of
the deck onto the top. For example, to cut -4:

| Top          Bottom
| 0 1 2 3 4 5 6 7 8 9   Your deck
| 
| 0 1 2 3 4 5           Your deck
|             6 7 8 9   Cut cards
| 
|         0 1 2 3 4 5   Your deck
| 6 7 8 9               Cut cards
| 
| 6 7 8 9 0 1 2 3 4 5   Your deck

To deal with increment N, start by clearing enough space on your table to lay
out all of the cards individually in a long line. Deal the top card into the
leftmost position. Then, move N positions to the right and deal the next card
there. If you would move into a position past the end of the space on your
table, wrap around and keep counting from the leftmost card again. Continue
this process until you run out of cards.

For example, to deal with increment 3:

| 
| 0 1 2 3 4 5 6 7 8 9   Your deck
| . . . . . . . . . .   Space on table
| ^                     Current position
| 
| Deal the top card to the current position:
| 
|   1 2 3 4 5 6 7 8 9   Your deck
| 0 . . . . . . . . .   Space on table
| ^                     Current position
| 
| Move the current position right 3:
| 
|   1 2 3 4 5 6 7 8 9   Your deck
| 0 . . . . . . . . .   Space on table
|       ^               Current position
| 
| Deal the top card:
| 
|     2 3 4 5 6 7 8 9   Your deck
| 0 . . 1 . . . . . .   Space on table
|       ^               Current position
| 
| Move right 3 and deal:
| 
|       3 4 5 6 7 8 9   Your deck
| 0 . . 1 . . 2 . . .   Space on table
|             ^         Current position
| 
| Move right 3 and deal:
| 
|         4 5 6 7 8 9   Your deck
| 0 . . 1 . . 2 . . 3   Space on table
|                   ^   Current position
| 
| Move right 3, wrapping around, and deal:
| 
|           5 6 7 8 9   Your deck
| 0 . 4 1 . . 2 . . 3   Space on table
|     ^                 Current position
| 
| And so on:
| 
| 0 7 4 1 8 5 2 9 6 3   Space on table

Positions on the table which already contain cards are still counted; they're
not skipped. Of course, this technique is carefully designed so it will never
put two cards in the same position or leave a position empty.

Finally, collect the cards on the table so that the leftmost card ends up at
the top of your deck, the card to its right ends up just below the top card,
and so on, until the rightmost card ends up at the bottom of the deck.

The complete shuffle process (your puzzle input) consists of applying many of
these techniques. Here are some examples that combine techniques; they all
start with a factory order deck of 10 cards:

| deal with increment 7
| deal into new stack
| deal into new stack
| Result: 0 3 6 9 2 5 8 1 4 7

| cut 6
| deal with increment 7
| deal into new stack
| Result: 3 0 7 4 1 8 5 2 9 6

| deal with increment 7
| deal with increment 9
| cut -2
| Result: 6 3 0 7 4 1 8 5 2 9

| deal into new stack
| cut -2
| deal with increment 7
| cut 8
| cut -4
| deal with increment 7
| cut 3
| deal with increment 9
| deal with increment 3
| cut -1
| Result: 9 2 5 8 1 4 7 0 3 6

Positions within the deck count from 0 at the top, then 1 for the card
immediately below the top card, and so on to the bottom. (That is, cards start
in the position matching their number.)

After shuffling your factory order deck of 10007 cards, what is the position of
card 2019?

-- Part Two ---

After a while, you realize your shuffling skill won't improve much more with
merely a single deck of cards. You ask every 3D printer on the ship to make you
some more cards while you check on the ship repairs. While reviewing the work
the droids have finished so far, you think you see Halley's Comet fly past!

When you get back, you discover that the 3D printers have combined their power
to create for you a single, giant, brand new, factory order deck of
119315717514047 space cards.

Finally, a deck of cards worthy of shuffling!

You decide to apply your complete shuffle process (your puzzle input) to the
deck 101741582076661 times in a row.

You'll need to be careful, though - one wrong move with this many cards and you
might overflow your entire ship!

After shuffling your new, giant, factory order deck that many times, what
number is on the card that ends up in position 2020?
-}

-- This will probably be too slow, but for 10007 cards, it's not too slow.
import Data.List(sort)

shuffle :: Int -> ([Int],[String]) -> ([Int],[String])
shuffle len (deck,("deal":"into":"new":"stack":rest)) = (reverse deck,rest)
shuffle len (deck,("cut":nstring:rest))
  | n >= 0 = (b++a,rest)
  | otherwise = (z++y,rest)
  where
    n = read nstring
    (a,b) = splitAt n deck
    (y,z) = splitAt (len+n) deck
shuffle len (deck,("deal":"with":"increment":nstring:rest)) =
    (map snd $ sort $ zip newpos deck,rest)
  where
    n = read nstring
    newpos = [(i*n) `mod` len | i <- [0..]]
shuffle len (deck,_) = (deck,[])

testData :: [([Int],String)]
testData = [
    ([0, 3, 6, 9, 2, 5, 8, 1, 4, 7],"deal with increment 7\ndeal into new stack\ndeal into new stack"),
    ([3, 0, 7, 4, 1, 8, 5, 2, 9, 6],"cut 6\ndeal with increment 7\ndeal into new stack"),
    ([6, 3, 0, 7, 4, 1, 8, 5, 2, 9],"deal with increment 7\ndeal with increment 9\ncut -2"),
    ([9, 2, 5, 8, 1, 4, 7, 0, 3, 6],"deal into new stack\ncut -2\ndeal with increment 7\ncut 8\ncut -4\ndeal with increment 7\ncut 3\ndeal with increment 9\ndeal with increment 3\ncut -1")
    ]

run :: Int -> String -> [Int]
run n input = fst $ head $ dropWhile (not . null . snd) $ iterate (shuffle n) ([0..n-1],words input)

test :: ()
test
  | any (uncurry (/=) . fmap (run 10) ) testData = error "a"
  | otherwise = ()

-- It's faster to just follow one card.
parse1 :: Int -> [String] -> Int -> Int
parse1 len ("deal":"into":"new":"stack":rest) = parse1 len rest . ((len - 1) -)
parse1 len ("cut":nstring:rest) = parse1 len rest . cut
  where
    n = read nstring
    cut i = (i - n) `mod` len
parse1 len ("deal":"with":"increment":nstring:rest) = parse1 len rest . deal
  where
    n = read nstring
    deal i = (i*n) `mod` len
parse1 _ _ = id

run1 :: Int -> Int -> String -> Int
run1 card len input = parse1 len (words input) card

part1 :: IO Int
part1 = fmap (run1 2019 10007) $ readFile "input/22.txt"

-- For part 2, invert the process to find where 2020th comes from, then
-- find where that comes from, etc, and find the periodicity.
parse2 :: Int -> [String] -> Int -> Int
parse2 len ("deal":"into":"new":"stack":rest) = ((len - 1) -) . parse2 len rest
parse2 len ("cut":nstring:rest) = uncut . parse2 len rest
  where
    n = read nstring
    uncut i = (i + n) `mod` len
parse2 len ("deal":"with":"increment":nstring:rest) = undeal . parse2 len rest
  where
    n = read nstring
    undeal i
      | i `mod` n /= 0 = undeal (i+len)
      | otherwise = i `div` n
parse2 _ _ = id

test2 :: IO ()
test2 = do
    input <- fmap words $ readFile "input/22.txt"
    let f = parse1 10007 input
    let finv = parse2 10007 input
    if (finv . f) 2019 /= 2019 then error "a" else return ()

-- The period is too long.  Have to resort to internet hints.

parse3 :: Integer -> [String] -> (Integer,Integer) -> (Integer,Integer)
parse3 len ("deal":"into":"new":"stack":rest) (a,b) = parse3 len rest ((-a) `mod` len, (-b-1) `mod` len)
parse3 len ("cut":nstring:rest) (a,b) = parse3 len rest (a, (b-n) `mod` len)
  where n = read nstring
parse3 len ("deal":"with":"increment":nstring:rest) (a,b) = parse3 len rest ((a*n)`mod`len, (b*n)`mod`len)
  where n = read nstring
parse3 _ _ ab = ab

apply3 :: Integer -> (Integer,Integer) -> Integer -> Integer
apply3 len (a,b) card = (a*card + b) `mod` len

powMod :: Integer -> Integer -> Integer -> Integer
powMod x y m = f (x `rem` m) y 1 `mod` m
  where
    f _ 0 acc = acc
    f b e acc = f (b * b `rem` m) (e `quot` 2)
      (if odd e then b * acc `rem` m else acc)

-- Taken from internet hints.  I don't know how this works.
invert3n :: Integer -> Integer -> (Integer,Integer) -> Integer -> Integer
invert3n len n (a,b) pos = ((pos - r)*powMod a (n*(len-2)) len + r) `mod` len
  where r = (b*powMod (1-a) (len-2) len) `mod` len

part2 :: IO ()
part2 = do
    input <- readFile "input/22.txt"
    let ab1 = parse3 10007 (words input) (1,0)
    print ("Part 1",apply3 10007 ab1 2019)
    let ab2 = parse3 119315717514047 (words input) (1,0)
    print ("Part 2",invert3n 119315717514047 101741582076661 ab2 2020)
