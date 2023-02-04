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
