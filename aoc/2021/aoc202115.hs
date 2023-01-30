{-
--- Day 15: Chiton ---

You've almost reached the exit of the cave, but the walls are getting closer
together. Your submarine can barely still fit, though; the main problem is that
the walls of the cave are covered in chitons, and it would be best not to bump
any of them.

The cavern is large, but has a very low ceiling, restricting your motion to two
dimensions. The shape of the cavern resembles a square; a quick scan of chiton
density produces a map of risk level throughout the cave (your puzzle input).
For example:

| 1163751742
| 1381373672
| 2136511328
| 3694931569
| 7463417111
| 1319128137
| 1359912421
| 3125421639
| 1293138521
| 2311944581

You start in the top left position, your destination is the bottom right
position, and you cannot move diagonally. The number at each position is its
risk level; to determine the total risk of an entire path, add up the risk
levels of each position you enter (that is, don't count the risk level of your
starting position unless you enter it; leaving it adds no risk to your total).

Your goal is to find a path with the lowest total risk. In this example, a
path with the lowest total risk is highlighted here:

| 1163751742
| 1381373672
| 2136511328
| 3694931569
| 7463417111
| 1319128137
| 1359912421
| 3125421639
| 1293138521
| 2311944581

The total risk of this path is 40 (the starting position is never entered, so
its risk is not counted).

What is the lowest total risk of any path from the top left to the bottom
right?

--- Part Two ---

Now that you know how to find low-risk paths in the cave, you can try to find
your way out.

The entire cave is actually five times larger in both dimensions than you
thought; the area you originally scanned is just one tile in a 5x5 tile area
that forms the full map. Your original map tile repeats to the right and
downward; each time the tile repeats to the right or downward, all of its risk
levels are 1 higher than the tile immediately up or left of it. However, risk
levels above 9 wrap back around to 1. So, if your original map had some
position with a risk level of 8, then that same position on each of the 25
total tiles would be as follows:

| 8 9 1 2 3
| 9 1 2 3 4
| 1 2 3 4 5
| 2 3 4 5 6
| 3 4 5 6 7

Each single digit above corresponds to the example position with a value of 8
on the top-left tile. Because the full map is actually five times larger in
both dimensions, that position appears a total of 25 times, once in each
duplicated tile, with the values shown above.

Here is the full five-times-as-large version of the first example above, with
the original map in the top left corner highlighted:

| 11637517422274862853338597396444961841755517295286
| 13813736722492484783351359589446246169155735727126
| 21365113283247622439435873354154698446526571955763
| 36949315694715142671582625378269373648937148475914
| 74634171118574528222968563933317967414442817852555
| 13191281372421239248353234135946434524615754563572
| 13599124212461123532357223464346833457545794456865
| 31254216394236532741534764385264587549637569865174
| 12931385212314249632342535174345364628545647573965
| 23119445813422155692453326671356443778246755488935
| 22748628533385973964449618417555172952866628316397
| 24924847833513595894462461691557357271266846838237
| 32476224394358733541546984465265719557637682166874
| 47151426715826253782693736489371484759148259586125
| 85745282229685639333179674144428178525553928963666
| 24212392483532341359464345246157545635726865674683
| 24611235323572234643468334575457944568656815567976
| 42365327415347643852645875496375698651748671976285
| 23142496323425351743453646285456475739656758684176
| 34221556924533266713564437782467554889357866599146
| 33859739644496184175551729528666283163977739427418
| 35135958944624616915573572712668468382377957949348
| 43587335415469844652657195576376821668748793277985
| 58262537826937364893714847591482595861259361697236
| 96856393331796741444281785255539289636664139174777
| 35323413594643452461575456357268656746837976785794
| 35722346434683345754579445686568155679767926678187
| 53476438526458754963756986517486719762859782187396
| 34253517434536462854564757396567586841767869795287
| 45332667135644377824675548893578665991468977611257
| 44961841755517295286662831639777394274188841538529
| 46246169155735727126684683823779579493488168151459
| 54698446526571955763768216687487932779859814388196
| 69373648937148475914825958612593616972361472718347
| 17967414442817852555392896366641391747775241285888
| 46434524615754563572686567468379767857948187896815
| 46833457545794456865681556797679266781878137789298
| 64587549637569865174867197628597821873961893298417
| 45364628545647573965675868417678697952878971816398
| 56443778246755488935786659914689776112579188722368
| 55172952866628316397773942741888415385299952649631
| 57357271266846838237795794934881681514599279262561
| 65719557637682166874879327798598143881961925499217
| 71484759148259586125936169723614727183472583829458
| 28178525553928963666413917477752412858886352396999
| 57545635726865674683797678579481878968159298917926
| 57944568656815567976792667818781377892989248891319
| 75698651748671976285978218739618932984172914319528
| 56475739656758684176786979528789718163989182927419
| 67554889357866599146897761125791887223681299833479

Equipped with the full map, you can now find a path from the top left corner
to the bottom right corner with the lowest total risk:

| 11637517422274862853338597396444961841755517295286
| 13813736722492484783351359589446246169155735727126
| 21365113283247622439435873354154698446526571955763
| 36949315694715142671582625378269373648937148475914
| 74634171118574528222968563933317967414442817852555
| 13191281372421239248353234135946434524615754563572
| 13599124212461123532357223464346833457545794456865
| 31254216394236532741534764385264587549637569865174
| 12931385212314249632342535174345364628545647573965
| 23119445813422155692453326671356443778246755488935
| 22748628533385973964449618417555172952866628316397
| 24924847833513595894462461691557357271266846838237
| 32476224394358733541546984465265719557637682166874
| 47151426715826253782693736489371484759148259586125
| 85745282229685639333179674144428178525553928963666
| 24212392483532341359464345246157545635726865674683
| 24611235323572234643468334575457944568656815567976
| 42365327415347643852645875496375698651748671976285
| 23142496323425351743453646285456475739656758684176
| 34221556924533266713564437782467554889357866599146
| 33859739644496184175551729528666283163977739427418
| 35135958944624616915573572712668468382377957949348
| 43587335415469844652657195576376821668748793277985
| 58262537826937364893714847591482595861259361697236
| 96856393331796741444281785255539289636664139174777
| 35323413594643452461575456357268656746837976785794
| 35722346434683345754579445686568155679767926678187
| 53476438526458754963756986517486719762859782187396
| 34253517434536462854564757396567586841767869795287
| 45332667135644377824675548893578665991468977611257
| 44961841755517295286662831639777394274188841538529
| 46246169155735727126684683823779579493488168151459
| 54698446526571955763768216687487932779859814388196
| 69373648937148475914825958612593616972361472718347
| 17967414442817852555392896366641391747775241285888
| 46434524615754563572686567468379767857948187896815
| 46833457545794456865681556797679266781878137789298
| 64587549637569865174867197628597821873961893298417
| 45364628545647573965675868417678697952878971816398
| 56443778246755488935786659914689776112579188722368
| 55172952866628316397773942741888415385299952649631
| 57357271266846838237795794934881681514599279262561
| 65719557637682166874879327798598143881961925499217
| 71484759148259586125936169723614727183472583829458
| 28178525553928963666413917477752412858886352396999
| 57545635726865674683797678579481878968159298917926
| 57944568656815567976792667818781377892989248891319
| 75698651748671976285978218739618932984172914319528
| 56475739656758684176786979528789718163989182927419
| 67554889357866599146897761125791887223681299833479

The total risk of this path is 315 (the starting position is still never
entered, so its risk is not counted).

Using the full map, what is the lowest total risk of any path from the top left
to the bottom right?
-}

import Debug.Trace(traceShow)

import Data.List(sort)
import Data.Map(Map,delete,empty,findWithDefault,fromList,insert,keysSet,member,minViewWithKey,union,(!))
import qualified Data.Map

parse :: String -> Map (Int,Int) Int
parse = fromList . p 0 0
  where
    p x y ('\n':rest) = p 0 (y+1) rest
    p x y (c:rest) = ((x,y),read [c]) : p (x+1) y rest
    p x y _ = []

-- Depth-first search
-- This is really slow
search :: Map (Int,Int) Int -> (Maybe Int,Map (Int,Int) Int) -> (Int,(Int,Int)) -> (Maybe Int,Map (Int,Int) Int)
search risks (bound,visited) (risk,xy@(x,y))
  | maybe False (newRisk >=) bound = (bound,visited)
  | xy == maximum (keysSet risks) = (Just newRisk,visited)
  | maybe False (newRisk >=) (Data.Map.lookup xy visited) = (bound,visited)
  | otherwise = foldl (search risks) (bound,insert xy newRisk visited) nexts
  where
    newRisk = risk + risks!xy
    nexts = map snd $ sort [((-newX-newY,risks!newXY),(newRisk,newXY)) | newXY@(newX,newY) <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)], member newXY risks]

onMap1 :: Map (Int,Int) Int -> (Int,Int) -> Bool
onMap1 = flip member

getRisk1 :: Map (Int,Int) Int -> (Int,Int) -> Int
getRisk1 = (!)

getGoal1 :: Map (Int,Int) Int -> (Int,Int)
getGoal1 = maximum . keysSet

run :: String -> Int
run input = minRisk
  where
    risks = parse input
    (Just minRisk,_) = search risks (Nothing,empty) (-risks!(0,0),(0,0))

testData :: String
testData = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581\n"

test :: ()
test
  | run testData /= 40 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap run $ readFile "input/15.txt"

onMap2 :: Map (Int,Int) Int -> (Int,Int) -> Bool
onMap2 risks (x,y) = x >= 0 && x < 5*(xmax+1) && y >= 0 && y < 5*(ymax+1)
  where (xmax,ymax) = maximum $ keysSet risks

getRisk2 :: Map (Int,Int) Int -> (Int,Int) -> Int
getRisk2 risks (x,y)
  | not (onMap2 risks (x,y)) = error (show (x,y))
  | otherwise = 1 + (risks!(x `mod` xsize,y `mod` ysize) - 1 + x `div` xsize + y `div` ysize) `mod` 9
  where
    (xmax,ymax) = maximum $ keysSet risks
    (xsize,ysize) = (xmax+1,ymax+1)

getGoal2 :: Map (Int,Int) Int -> (Int,Int)
getGoal2 risks = (5*xmax+4,5*ymax+4)
  where (xmax,ymax) = maximum $ keysSet risks

-- This is still way too slow for part 2, but it's almost fast enough
-- for part 1.  A depth-first search is probably the wrong approach.

type Best = (Int,Path,Map (Int,Int) Int)
type Path = [((Int,Int),Int)]

search2 :: ((Int,Int) -> Bool,(Int,Int) -> Int,(Int,Int)) -> Path -> (Best,Map (Int,Int) Int) -> (Best,Map (Int,Int) Int)
search2 risks@(onMap,getRisk,goal) ((xy@(x,y),risk):path) (best@(bestRisk,bestPath,bestPoints),visited)
  | newRisk >= bestRisk = (best,visited)
  | maybe False (newRisk >=) (Data.Map.lookup xy visited) = (best,visited)
  | xy == goal = traceShow ("goal",xy,newRisk,take 5 path) ((newRisk,path,fromList path),visited)
  | otherwise = foldr (search2 risks) nextBestVisited nextPaths
  where
    newRisk = risk + getRisk xy
    nextPaths = map ((:(xy,newRisk):path) . snd) $ sort [((newX+newY,-getRisk newXY),(newXY,newRisk)) | newXY@(newX,newY) <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)], onMap newXY]
    nextBestVisited
      | not (member xy bestPoints) = (best,insert xy newRisk visited)
      | otherwise = traceShow ("imp",xy,improvement,bestRisk+improvement) ((bestRisk+improvement,newBestPath++(xy,newRisk):path,newBestPoints),union {- elems in 1st arg override elems in 2nd arg -} newBestVisited visited)
        where
          improvement = newRisk - bestPoints!xy
          newBestPath = map (fmap (+improvement)) $ takeWhile ((/= xy) . fst) bestPath
          newBestVisited = fromList ((xy,newRisk):newBestPath)
          newBestPoints = fromList (newBestPath ++ (xy,newRisk):path)

run2small :: String -> Int
run2small input = minRisk
  where
    risks = parse input
    ((minRisk,_,_),_) = search2 (onMap1 risks,getRisk1 risks,getGoal1 risks) [((0,0),-risks!(0,0))] ((sum risks,[],empty),empty)

run2 :: String -> Int
run2 input = minRisk
  where
    risks = parse input
    ((minRisk,_,_),_) = search2 (onMap2 risks,getRisk2 risks,getGoal2 risks) [((0,0),-risks!(0,0))] ((25*sum risks,[],empty),empty)

test2 :: ()
test2
  | run2small testData /= 40 = error "a"
  | run2 testData /= 315 = error "b"
  | otherwise = ()

-- https://en.wikipedia.org/wiki/A*_search_algorithm
-- Still too slow, but fast enough to get the answer.
search3 :: ((Int,Int) -> Bool,(Int,Int) -> Int,(Int,Int)) -> (Int,Int) -> Int
search3 (onMap,getRisk,goal@(goalX,goalY)) start@(startX,startY) =
    astar (fromList [((h start,start),())],fromList [(start,h start)],fromList [(start,0)])
  where
    h :: (Int,Int) -> Int
    h xy@(x,y) = abs (x-goalX) + abs (y-goalY)

    astar :: (Map (Int,(Int,Int)) (),Map (Int,Int) Int,Map (Int,Int) Int) -> Int
    astar (openQ,openSet,scores)
      | (Data.Map.size openQ) `mod` 100 == 0 && traceShow (Data.Map.size openQ,xy) False = undefined
      | xy == goal = currentScore
      | otherwise = astar $ foldr (handleNeighbor currentScore) (newOpenQ,newOpenSet,scores) [newXY | newXY <- [(x-1,y),(x,y-1),(x+1,y),(x,y+1)], onMap newXY]
      where
        Just (((_,xy@(x,y)),()),newOpenQ) = minViewWithKey openQ
        newOpenSet = delete xy openSet
        currentScore = scores!xy

    handleNeighbor :: Int -> (Int,Int) -> (Map (Int,(Int,Int)) (),Map (Int,Int) Int,Map (Int,Int) Int) -> (Map (Int,(Int,Int)) (),Map (Int,Int) Int,Map (Int,Int) Int)
    handleNeighbor currentScore xy (openQ,openSet,scores)
      | maybe False ((currentScore+getRisk xy)>=) $ Data.Map.lookup xy scores = (openQ,openSet,scores)
      | member xy openSet = (insert ((currentScore+h xy),xy) () $ delete (openSet!xy,xy) openQ,insert xy (currentScore+h xy) openSet,insert xy (currentScore+getRisk xy) scores)
      | otherwise = (insert ((currentScore+h xy),xy) () openQ,insert xy (currentScore+h xy) openSet,insert xy (currentScore+getRisk xy) scores)

run3small :: String -> Int
run3small input = search3 (onMap1 risks,getRisk1 risks,getGoal1 risks) (0,0)
  where risks = parse input

run3 :: String -> Int
run3 input = search3 (onMap2 risks,getRisk2 risks,getGoal2 risks) (0,0)
  where risks = parse input

test3 :: ()
test3
  | run3small testData /= 40 = error "a"
  | run3 testData /= 315 = error "b"
  | otherwise = ()

part2small :: IO Int
part2small = fmap run3small $ readFile "input/15.txt"

part2 :: IO Int
part2 = fmap run3 $ readFile "input/15.txt"
