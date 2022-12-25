{-
--- Day 14: Reindeer Olympics ---

This year is the Reindeer Olympics! Reindeer can fly at high speeds, but must
rest occasionally to recover their energy. Santa would like to know which of
his reindeer is fastest, and so he has them race.

Reindeer can only either be flying (always at their top speed) or resting (not
moving at all), and always spend whole seconds in either state.

For example, suppose you have the following Reindeer:

 - Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
 - Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.

After one second, Comet has gone 14 km, while Dancer has gone 16 km. After ten
seconds, Comet has gone 140 km, while Dancer has gone 160 km. On the eleventh
second, Comet begins resting (staying at 140 km), and Dancer continues on for a
total distance of 176 km. On the 12th second, both reindeer are resting. They
continue to rest until the 138th second, when Comet flies for another ten
seconds. On the 174th second, Dancer flies for another 11 seconds.

In this example, after the 1000th second, both reindeer are resting, and Comet
is in the lead at 1120 km (poor Dancer has only gotten 1056 km by that point).
So, in this situation, Comet would win (if the race ended at 1000 seconds).

Given the descriptions of each reindeer (in your puzzle input), after exactly
2503 seconds, what distance has the winning reindeer traveled?
-}

dist :: Int -> (Int,Int,Int) -> Int
dist totalTime (speed,fly,rest) = cycles*fly*speed + (min fly remainder)*speed
  where
    (cycles,remainder) = totalTime `divMod` (fly+rest)

parse :: String -> [(Int,Int,Int)]
parse descriptions = p (words descriptions)
  where
    p (_:_:_:speed:_:_:fly:_:_:_:_:_:_:rest:_:remaining) = (read speed,read fly,read rest) : p remaining
    p [] = []

test :: ()
test
  | map (dist 1) reindeer /= [14,16] = error "a"
  | map (dist 10) reindeer /= [140,160] = error "b"
  | map (dist 11) reindeer /= [140,176] = error "c"
  | map (dist 12) reindeer /= [140,176] = error "d"
  | map (dist 137) reindeer /= [140,176] = error "e"
  | map (dist 147) reindeer /= [280,176] = error "f"
  | map (dist 173) reindeer /= [280,176] = error "g"
  | map (dist 184) reindeer /= [280,352] = error "h"
  | map (dist 1000) reindeer /= [1120,1056] = error "i"
  | otherwise = ()
  where
    reindeer = parse "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\nDancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."

part1 :: IO Int
part1 = fmap (maximum . map (dist 2503) . parse) (readFile "input/14.txt")

data State = Fly Int | Rest Int deriving Show

initReindeer :: (Int,Int,Int) -> (Int,(Int,State,Int,Int,Int))
initReindeer (speed,fly,rest) = (0,(0,Fly fly,speed,fly,rest))

score :: Int -> (Int,(Int,State,Int,Int,Int)) -> (Int,(Int,State,Int,Int,Int))
score lead (dist,(points,state,speed,fly,rest))
  | dist == lead = (dist,(points+1,state,speed,fly,rest))
  | otherwise = (dist,(points,state,speed,fly,rest))

advance :: (Int,(Int,State,Int,Int,Int)) -> (Int,(Int,State,Int,Int,Int))
advance (dist,(points,Fly n,speed,fly,rest))
  | n > 0 = (dist+speed,(points,Fly (n-1),speed,fly,rest))
  | otherwise = (dist,(points,Rest (rest-1),speed,fly,rest))
advance (dist,(points,Rest n,speed,fly,rest))
  | n > 0 = (dist,(points,Rest (n-1),speed,fly,rest))
  | otherwise = (dist+speed,(points,Fly (fly-1),speed,fly,rest))

step :: [(Int,(Int,State,Int,Int,Int))] -> [(Int,(Int,State,Int,Int,Int))]
step field = map (score (maximum (map fst next))) next
  where next = map advance field

getScore :: (Int,(Int,State,Int,Int,Int)) -> Int
getScore (_,(points,_,_,_,_)) = points

part2 :: IO Int
part2 = fmap (maximum . map getScore . head . drop 2503 . iterate step . map initReindeer . parse) (readFile "input/14.txt")
