{-
--- Day 13: Shuttle Search ---

Your ferry can make it safely to a nearby port, but it won't get much further.
When you call to book another ship, you discover that no ships embark from that
port to your vacation island. You'll need to get from the port to the nearest
airport.

Fortunately, a shuttle bus service is available to bring you from the sea port
to the airport! Each bus has an ID number that also indicates how often the bus
leaves for the airport.

Bus schedules are defined based on a timestamp that measures the number of
minutes since some fixed reference point in the past. At timestamp 0, every bus
simultaneously departed from the sea port. After that, each bus travels to the
airport, then various other locations, and finally returns to the sea port to
repeat its journey forever.

The time this loop takes a particular bus is also its ID number: the bus with
ID 5 departs from the sea port at timestamps 0, 5, 10, 15, and so on. The bus
with ID 11 departs at 0, 11, 22, 33, and so on. If you are there when the bus
departs, you can ride that bus to the airport!

Your notes (your puzzle input) consist of two lines. The first line is your
estimate of the earliest timestamp you could depart on a bus. The second line
lists the bus IDs that are in service according to the shuttle company; entries
that show x must be out of service, so you decide to ignore them.

To save time once you arrive, your goal is to figure out the earliest bus you
can take to the airport. (There will be exactly one such bus.)

For example, suppose you have the following notes:

| 939
| 7,13,x,x,59,x,31,19

Here, the earliest timestamp you could depart is 939, and the bus IDs in
service are 7, 13, 59, 31, and 19. Near timestamp 939, these bus IDs depart at
the times marked D:

| time   bus 7   bus 13  bus 59  bus 31  bus 19
| 929      .       .       .       .       .
| 930      .       .       .       D       .
| 931      D       .       .       .       D
| 932      .       .       .       .       .
| 933      .       .       .       .       .
| 934      .       .       .       .       .
| 935      .       .       .       .       .
| 936      .       D       .       .       .
| 937      .       .       .       .       .
| 938      D       .       .       .       .
| 939      .       .       .       .       .
| 940      .       .       .       .       .
| 941      .       .       .       .       .
| 942      .       .       .       .       .
| 943      .       .       .       .       .
| 944      .       .       D       .       .
| 945      D       .       .       .       .
| 946      .       .       .       .       .
| 947      .       .       .       .       .
| 948      .       .       .       .       .
| 949      .       D       .       .       .

The earliest bus you could take is bus ID 59. It doesn't depart until timestamp
944, so you would need to wait 944 - 939 = 5 minutes before it departs.
Multiplying the bus ID by the number of minutes you'd need to wait gives 295.

What is the ID of the earliest bus you can take to the airport multiplied by
the number of minutes you'll need to wait for that bus?

--- Part Two ---

The shuttle company is running a contest: one gold coin for anyone that can
find the earliest timestamp such that the first bus ID departs at that time and
each subsequent listed bus ID departs at that subsequent minute. (The first
line in your input is no longer relevant.)

For example, suppose you have the same list of bus IDs as above:

| 7,13,x,x,59,x,31,19

An x in the schedule means there are no constraints on what bus IDs must departat that time.

This means you are looking for the earliest timestamp (called t) such that:

 - Bus ID 7 departs at timestamp t.
 - Bus ID 13 departs one minute after timestamp t.
 - There are no requirements or restrictions on departures at two or three
   minutes after timestamp t.
 - Bus ID 59 departs four minutes after timestamp t.
 - There are no requirements or restrictions on departures at five minutes
   after timestamp t.
 - Bus ID 31 departs six minutes after timestamp t.
 - Bus ID 19 departs seven minutes after timestamp t.

The only bus departures that matter are the listed bus IDs at their specific
offsets from t. Those bus IDs can depart at other times, and other bus IDs can
depart at those times. For example, in the list above, because bus ID 19 must
depart seven minutes after the timestamp at which bus ID 7 departs, bus ID 7
will always also be departing with bus ID 19 at seven minutes after timestamp
t.

In this example, the earliest timestamp at which this occurs is 1068781:

| time     bus 7   bus 13  bus 59  bus 31  bus 19
| 1068773    .       .       .       .       .
| 1068774    D       .       .       .       .
| 1068775    .       .       .       .       .
| 1068776    .       .       .       .       .
| 1068777    .       .       .       .       .
| 1068778    .       .       .       .       .
| 1068779    .       .       .       .       .
| 1068780    .       .       .       .       .
| 1068781    D       .       .       .       .
| 1068782    .       D       .       .       .
| 1068783    .       .       .       .       .
| 1068784    .       .       .       .       .
| 1068785    .       .       D       .       .
| 1068786    .       .       .       .       .
| 1068787    .       .       .       D       .
| 1068788    D       .       .       .       D
| 1068789    .       .       .       .       .
| 1068790    .       .       .       .       .
| 1068791    .       .       .       .       .
| 1068792    .       .       .       .       .
| 1068793    .       .       .       .       .
| 1068794    .       .       .       .       .
| 1068795    D       D       .       .       .
| 1068796    .       .       .       .       .
| 1068797    .       .       .       .       .

In the above example, bus ID 7 departs at timestamp 1068788 (seven minutes
after t). This is fine; the only requirement on that minute is that bus ID 19
departs then, and it does.

Here are some other examples:

 - The earliest timestamp that matches the list 17,x,13,19 is 3417.
 - 67,7,59,61 first occurs at timestamp 754018.
 - 67,x,7,59,61 first occurs at timestamp 779210.
 - 67,7,x,59,61 first occurs at timestamp 1261476.
 - 1789,37,47,1889 first occurs at timestamp 1202161486.
However, with so many bus IDs in your list, surely the actual earliest
timestamp will be larger than 100000000000000!

What is the earliest timestamp such that all of the listed bus IDs depart at
offsets matching their positions in the list?
-}

import Data.Char(isDigit)
import Data.Ratio(denominator)

parse1 :: String -> (Int,[Int])
parse1 str = (head list,tail list)
  where
    list = (map read . words . map toSpace) str
    toSpace c | c `elem` ",x" = ' ' | otherwise = c

addWait :: Int -> Int -> (Int,Int)
addWait t0 busID = ((-t0) `mod` busID,busID)

addWaits :: (Int,[Int]) -> [(Int,Int)]
addWaits (t0,busIDs) = map (addWait t0) busIDs

testData :: String
testData = "939\n7,13,x,x,59,x,31,19\n"

test :: ()
test
  | (uncurry (*) . minimum . addWaits . parse1) testData /= 295 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (uncurry (*) . minimum . addWaits . parse1) $ readFile "input/13.txt"

parse2 :: String -> [(Int,Int)]
parse2 = p 0 . dropWhile isDigit
  where
    p i "" = []
    p i "\n" = []
    p i (_:'x':rest) = p (i+1) rest
    p i str = (i,read n) : p (i+1) rest
      where (n,rest) = span isDigit (drop 1 str)

-- Given a,x,b,x,x,x,c
-- look for n*a + 2 = m*b, (n*a + 2) mod b = 0
-- then look for n*a + 5 = p*c, (n*a + 5) mod c = 0

-- In general, look for n*busid[0] + delay[i] = m[i]*busid[i]
-- where n and m[i] are integers.
-- (n*busid[0] + delay[i]) mod busid[i] = 0

-- This is very slow, but gives the right results for the test cases.
findT :: [(Int,Int)] -> [Int]
findT ((0,busid0):buses) = map (busid0*) $ foldr sieve [1..] buses
  where
    sieve (i,busid) ns = filter (f i busid) ns
    f i busid n = (n*busid0 + i) `mod` busid == 0

-- Internet hints point to
-- https://en.wikipedia.org/wiki/Chinese_remainder_theorem
-- I note that all the busids are prime, making dividing out the gcd moot.
findT2 :: [(Int,Int)] -> Int
findT2 buses = s - nn*(s `div` nn)
  where
    s = sum $ map f buses
    nn = foldr prod 1 (map snd buses)
    prod a b = a*b `div` gcd a b
    f (delay,busid) = (nn `div` busid)*(g (nn `div` busid) busid ((-delay) `mod` busid))
    g a b c = search 0
      -- find x such that mod (a*x) b = c
      -- minimum x means x < b
      where search x | mod (a*x) b == c = x | otherwise = search (x+1)

testData2 :: [String]
testData2 = ["939\n7,13,x,x,59,x,31,19\n","3417 17,x,13,19","754018 67,7,59,61","779210 67,x,7,59,61","1261476 67,7,x,59,61","1202161486 1789,37,47,1889"]

test2 :: ()
test2
  | (findT2 . parse2) (testData2 !! 0) /= 1068781 = error "a"
  | (findT2 . parse2) (testData2 !! 1) /= 3417 = error "b"
  | (findT2 . parse2) (testData2 !! 2) /= 754018 = error "c"
  | (findT2 . parse2) (testData2 !! 3) /= 779210 = error "d"
  | (findT2 . parse2) (testData2 !! 4) /= 1261476 = error "e"
  | (findT2 . parse2) (testData2 !! 5) /= 1202161486 = error "f"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (findT2 . parse2) $ readFile "input/13.txt"
