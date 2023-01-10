{-
--- Day 4: Repose Record ---

You've sneaked into another supply closet - this time, it's across from the
prototype suit manufacturing lab. You need to sneak inside and fix the issues
with the suit, but there's a guard stationed outside the lab, so this is as
close as you can safely get.

As you search the closet for anything that might help, you discover that you're
not the first person to want to sneak in. Covering the walls, someone has spent
an hour starting every midnight for the past few months secretly observing this
guard post! They've been writing down the ID of the one guard on duty that
night - the Elves seem to have decided that one guard was enough for the
overnight shift - as well as when they fall asleep or wake up while at their
post (your puzzle input).

For example, consider the following records, which have already been organized
into chronological order:

| [1518-11-01 00:00] Guard #10 begins shift
| [1518-11-01 00:05] falls asleep
| [1518-11-01 00:25] wakes up
| [1518-11-01 00:30] falls asleep
| [1518-11-01 00:55] wakes up
| [1518-11-01 23:58] Guard #99 begins shift
| [1518-11-02 00:40] falls asleep
| [1518-11-02 00:50] wakes up
| [1518-11-03 00:05] Guard #10 begins shift
| [1518-11-03 00:24] falls asleep
| [1518-11-03 00:29] wakes up
| [1518-11-04 00:02] Guard #99 begins shift
| [1518-11-04 00:36] falls asleep
| [1518-11-04 00:46] wakes up
| [1518-11-05 00:03] Guard #99 begins shift
| [1518-11-05 00:45] falls asleep
| [1518-11-05 00:55] wakes up

Timestamps are written using year-month-day hour:minute format. The guard
falling asleep or waking up is always the one whose shift most recently
started. Because all asleep/awake times are during the midnight hour (00:00 -
00:59), only the minute portion (00 - 59) is relevant for those events.

Visually, these records show that the guards are asleep at these times:

| Date   ID   Minute
|             000000000011111111112222222222333333333344444444445555555555
|             012345678901234567890123456789012345678901234567890123456789
| 11-01  #10  .....####################.....#########################.....
| 11-02  #99  ........................................##########..........
| 11-03  #10  ........................#####...............................
| 11-04  #99  ....................................##########..............
| 11-05  #99  .............................................##########.....

The columns are Date, which shows the month-day portion of the relevant day;
ID, which shows the guard on duty that day; and Minute, which shows the minutes
during which the guard was asleep within the midnight hour. (The Minute
column's header shows the minute's ten's digit in the first row and the one's
digit in the second row.) Awake is shown as ., and asleep is shown as #.

Note that guards count as asleep on the minute they fall asleep, and they count
as awake on the minute they wake up. For example, because Guard #10 wakes up at
00:25 on 1518-11-01, minute 25 is marked as awake.

If you can figure out the guard most likely to be asleep at a specific time,
you might be able to trick that guard into working tonight so you can have the
best chance of sneaking in. You have two strategies for choosing the best
guard/minute combination.

Strategy 1: Find the guard that has the most minutes asleep. What minute does
that guard spend asleep the most?

In the example above, Guard #10 spent the most minutes asleep, a total of 50
minutes (20+25+5), while Guard #99 only slept for a total of 30 minutes
(10+10+10). Guard #10 was asleep most during minute 24 (on two days, whereas
any other minute the guard was asleep was only seen on one day).

While this example listed the entries in chronological order, your entries are
in the order you found them. You'll need to organize them before they can be
analyzed.

What is the ID of the guard you chose multiplied by the minute you chose? (In
the above example, the answer would be 10 * 24 = 240.)
-}

import Data.List(sort)
import Data.Map(Map,alter,empty,findWithDefault,insert,size,toList,(!))
import Data.Tuple(swap)

type Log = ((Int,Int,Int,Int,Int),[String])

parse :: String -> [Log]
parse ('[':y1:y2:y3:y4:'-':m1:m2:'-':d1:d2:' ':h1:h2:':':min1:min2:']':' ':str) = ((read [y1,y2,y3,y4],read [m1,m2],read [d1,d2],read [h1,h2],read [min1,min2]),words log) : parse rest
  where (log,rest) = span (/= '[') str
parse _ = []

tabulate :: [Log] -> Map Int (Map Int Int)
tabulate logs = closeLogs $ foldl processLog (0,0,empty,empty) logs
  where
    closeLogs (currentGuard,_,currentTable,allGuards) =
        insert currentGuard currentTable allGuards
    processLog (currentGuard,sleepMinute,currentTable,allGuards)
               (_,("Guard":('#':newGuardStr):_))
      | currentGuard == newGuard = (newGuard,0,currentTable,allGuards)
      | otherwise = (newGuard,0,findWithDefault empty newGuard allGuards,insert currentGuard currentTable allGuards)
      where
        newGuard = read newGuardStr
    processLog (currentGuard,sleepMinute,currentTable,allGuards)
               ((_,_,_,_,minute),("falls":_)) =
        (currentGuard,minute,currentTable,allGuards)
    processLog (currentGuard,sleepMinute,currentTable,allGuards)
               ((_,_,_,_,minute),("wakes":_)) =
        (currentGuard,minute,foldl addNap currentTable [sleepMinute..minute-1],allGuards)
      where addNap table m = alter (Just . maybe 1 (+1)) m table

strategy1 :: Map Int (Map Int Int) -> (Int,Int)
strategy1 table = findMinute $ maximum $ map getTotal $ toList table
  where
    getTotal (guard,guardTable) = (sum guardTable,(guard,guardTable))
    findMinute (_,(guard,guardTable)) = (guard,snd $ maximum $ map swap $ toList guardTable)

testData = "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up"

test :: ()
test
  | sum (table!10) /= 50 = error "a"
  | sum (table!99) /= 30 = error "b"
  | snd (maximum $ map swap $ toList (table!10)) /= 24 = error "c"
  | strategy1 table /= (10,24) = error "d"
  | otherwise = ()
  where
    table = tabulate $ sort $ parse testData

part1 :: IO Int
part1 = fmap (uncurry (*) . strategy1 . tabulate . sort . parse) $ readFile "input/04.txt"

strategy2 :: Map Int (Map Int Int) -> Int
strategy2 table = guard*minute
  where
    ((_,minute),guard) = maximum $ map (swap . fmap (maximum . map swap . toList)) $ filter ((> 0) . size . snd) $ toList table

test2 :: ()
test2
  | strategy2 (tabulate $ sort $ parse testData) /= 4455 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (strategy2 . tabulate . sort . parse) $ readFile "input/04.txt"
