import Data.List(sort)
import Data.Map(Map,delete,empty,findWithDefault,fromList,insert,keysSet,mapWithKey,minViewWithKey,toList,union,(!))
import qualified Data.Map
import Data.Set(Set,difference,elems,member,singleton,size)
import qualified Data.Set

parse :: String -> Map (Int,Int) Char
parse = fromList . p 0 0
  where
    p x y [] = []
    p x y ('\n':rest) = p 0 (y+1) rest
    p x y (c:rest)
      | c `elem` ".ABCD" = ((x,y),c) : p (x+1) y rest
      | otherwise = p (x+1) y rest

getGoals :: Map (Int,Int) Char -> Set (Char,(Int,Int))
getGoals = Data.Set.fromList . scan 'A' . toList
  where
    -- this relies on the elements being sorted by increasing column, then
    -- by increasing row
    scan ty ((xy,ch):rest)
      | ch `elem` "ABCD" = (ty,xy) : scanRoom ty rest
      | otherwise = scan ty rest
    scan _ [] = []
    scanRoom ty ((xy,ch):rest)
      | ch `elem` "ABCD" = (ty,xy) : scanRoom ty rest
      | otherwise = scan (succ ty) rest
    scanRoom _ [] = []

getForbidden :: Set (Char,(Int,Int)) -> Set ((Int,Int),Char)
getForbidden goals = Data.Set.fromList $ concatMap toForbidden $ elems goals
  where
    toForbidden (ty,(x,y)) = [((x,y),t) | t <- "ABCD", t /= ty] ++ [((x,y-1),t) | t <- "ABCD", not (member (t,(x,y-1)) goals)]

finished :: Set (Char,(Int,Int)) -> Map (Int,Int) Char -> Bool
finished goals = all atGoal . toList
  where
    atGoal (xy,ch)
      | ch `elem` "ABCD" = member (ch,xy) goals
      | otherwise = True

-- Bounded depth-first search, this is too slow
search :: Set (Char,(Int,Int)) -> Set ((Int,Int),Char) -> Map (Int,Int) Char -> Int -> Maybe Int -> [(Char,(Int,Int),Int)] -> Maybe Int
search goals forbidden burrow energy bound history
  | maybe False (energy >=) bound = bound
  | finished goals burrow = Just energy
  | otherwise = foldr makeMove bound moves
  where
    moves = (reverse . sort . filter (not . isToHome)) allMoves ++ (reverse . sort . filter isToHome) allMoves
      where
        allMoves = concatMap getMoves $ toList burrow
        isToHome (_,ch,_,to) = isHome ch to

    makeMove (cost,ch,from,to) bound = search goals forbidden (insert to ch (insert from '.' burrow)) (energy+cost) bound ((ch,to,cost):history)

    getMoves (xy@(x,y),ch)
      | not (ch `elem` "ABCD") = []
      | member (ch,xy) goals && roomOk ch (x,y+1) = []
      | member (xy,ch) forbidden || member (ch,xy) goals = filter validMoveOut $ moveOut ch xy empty 0 (singleton xy)
      | otherwise = moveHome ch xy Data.Set.empty 0 (singleton xy)
    roomOk ch (x,y)
      | not (member (ch,(x,y)) goals) = True
      | ch /= burrow!(x,y) = False
      | otherwise = roomOk ch (x,y+1)

    isHome ch xy@(x,y) = member (ch,xy) goals && roomOk ch (x,y+1)
    isEmpty xy = findWithDefault '#' xy burrow == '.'

    validMoveOut (_,ch,_,to)
      | member (to,ch) forbidden = False
      | isHome ch to = True
      | member (ch,to) goals = False
      | otherwise = True

    neighbors (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

    use ch energy
      | ch == 'A' = energy + 1
      | ch == 'B' = energy + 10
      | ch == 'C' = energy + 100
      | ch == 'D' = energy + 1000

    moveOut :: Char -> (Int,Int) -> Map (Int,Int) (Int,Char,(Int,Int),(Int,Int)) -> Int -> Set (Int,Int) -> [(Int,Char,(Int,Int),(Int,Int))]
    moveOut ch xy0 seen cost xys
      | size xys == 0 = Data.Map.elems seen
      | any (isHome ch) xys = map toMoveHome $ filter (isHome ch) $ elems xys
      | otherwise = moveOut ch xy0 (union seen $ fromList $ map toMove $ elems newXYs) (use ch cost) newXYs
      where
        newXYs = difference (Data.Set.fromList $ filter isEmpty $ concatMap neighbors $ elems xys) (keysSet seen)
        toMove to = (to,(use ch cost,ch,xy0,to))
        toMoveHome to = (cost,ch,xy0,to)

    -- breadth-first
    moveHome :: Char -> (Int,Int) -> Set (Int,Int) -> Int -> Set (Int,Int) -> [(Int,Char,(Int,Int),(Int,Int))]
    moveHome ch xy0 seen cost xys
      | size xys == 0 = []
      | any (isHome ch) xys = map toMove $ filter (isHome ch) $ elems xys
      | otherwise = moveHome ch xy0 (Data.Set.union seen newXYs) (use ch cost) newXYs
      where
        toMove to = (cost,ch,xy0,to)
        newXYs = difference (Data.Set.fromList $ filter isEmpty $ concatMap neighbors $ elems xys) seen

run :: String -> Maybe Int
run input = search g (getForbidden g) b 0 Nothing []
  where
    b = parse input
    g = getGoals b

testData :: String
testData = concat [
    "#############\n",
    "#...........#\n",
    "###B#C#B#D###\n",
    "  #A#D#C#A#\n",
    "  #########\n"
    ]

-- This is too slow
{-
test :: ()
test
  | run testData /= Just 12521 = error "a"
  | otherwise = ()
-}

-- Note that the shape of the burrow in the input data is the same as that in
-- the example.
type State = (Map Int Char,Map Int (Char,Char))

toState :: Map (Int,Int) Char -> State
toState burrow = (fromList [(x,'.') | x <- [1..11]],fromList [
    (3,(burrow!(3,2),burrow!(3,3))),
    (5,(burrow!(5,2),burrow!(5,3))),
    (7,(burrow!(7,2),burrow!(7,3))),
    (9,(burrow!(9,2),burrow!(9,3)))])

searchA :: State -> Int
searchA initialState =
    astar ((fromList [((h initialState,initialState),())]),
           (fromList [(initialState,h initialState)]),
           (fromList [(initialState,0)]))
  where
    factor '.' = 0
    factor 'A' = 1
    factor 'B' = 10
    factor 'C' = 100
    factor 'D' = 1000
    home '.' = 0
    home 'A' = 3
    home 'B' = 5
    home 'C' = 7
    home 'D' = 9

    goal = fromList [(3,('A','A')),(5,('B','B')),(7,('C','C')),(9,('D','D'))]

    h :: State -> Int
    h (hall,homes) = sum (mapWithKey hHall hall) + sum (mapWithKey hHome homes)
    hHall x c = factor c*(abs (x - home c) + 1)
    hHome x (c1,c2)
      | home c1 == x && home c2 == x = 0
      | home c2 == x = factor c1*(abs (x - home c1) + 2)
      | home c1 == x = factor c2*(abs (x - home c2) + 4) + factor c1*5
      | otherwise = factor c1*(abs (x - home c1) + 2) + factor c2*(abs (x - home c2) + 3)

    neighbors :: State -> [(Int,State)]
    neighbors state@(hall,homes) = concatMap (moveHome state) (toList hall) ++ concatMap (moveOut state) (toList homes)

    moveHome :: State -> (Int,Char) -> [(Int,State)]
    moveHome state@(hall,homes) (x,c)
      | c == '.' = []
      | home1 /= '.' = []
      | home2 /= '.' && home2 /= c = []
      | home c > x && and [hall!xx == '.' | xx <- [x+1..home c]] =
          if home2 == '.'
            then [(factor c*(home c-x+2),(insert x '.' hall,insert (home c) ('.',c) homes))]
            else [(factor c*(home c-x+1),(insert x '.' hall,insert (home c) (c,home2) homes))]
      | home c < x && and [hall!xx == '.' | xx <- [home c..x-1]] =
          if home2 == '.'
            then [(factor c*(x-home c+2),(insert x '.' hall,insert (home c) ('.',c)homes ))]
            else [(factor c*(x-home c+1),(insert x '.' hall,insert (home c) (c,home2) homes))]
      | otherwise = []
      where
        (home1,home2) = homes!(home c)

    moveOut :: State -> (Int,(Char,Char)) -> [(Int,State)]
    moveOut state@(hall,homes) (x,(c1,c2))
      | home c1 == x && home c2 == x = []
      | home c2 == x && c1 == '.' = []
      | c1 == '.' && c2 == '.' = []
      | c1 /= '.' = [(factor c1*(abs (x-xx)+1),(insert xx c1 hall,insert x ('.',c2) homes)) | xx <- destXs]
      | otherwise = [(factor c2*(abs (x-xx)+2),(insert xx c2 hall,insert x ('.','.') homes)) | xx <- destXs]
      where
        destXs = takeWhile ((== '.') . (hall!)) [xx | xx <- [x+1..11], xx /= 3, xx /= 5, xx /= 7, xx /= 9] ++ takeWhile ((== '.') . (hall!)) [xx | xx <- [x-1,x-2..1], xx /= 3, xx /= 5, xx /= 7, xx /= 9]

    astar :: (Map (Int,State) (),Map State Int,Map State Int) -> Int
    astar (openQ,openSet,scores)
      | currentHomes == goal = currentScore
      | otherwise = astar $ foldr (handleNeighbor currentScore) (newOpenQ,newOpenSet,scores) (neighbors current)
      where
        Just (((_,current@(_,currentHomes)),()),newOpenQ) = minViewWithKey openQ
        newOpenSet = delete current openSet
        currentScore = scores!current

    handleNeighbor :: Int -> (Int,State) -> (Map (Int,State) (),Map State Int,Map State Int) -> (Map (Int,State) (),Map State Int,Map State Int)
    handleNeighbor currentScore (dscore,state) (openQ,openSet,scores)
      | maybe False ((currentScore+dscore) >=) $ Data.Map.lookup state scores = (openQ,openSet,scores)
      | Data.Map.member state openSet = (insert ((currentScore+dscore+h state),state) () $ delete (openSet!state,state) openQ,insert state (currentScore+dscore+h state) openSet,insert state (currentScore+dscore) scores)
      | otherwise = (insert ((currentScore+dscore+h state),state) () openQ,insert state (currentScore+dscore+h state) openSet,insert state (currentScore+dscore) scores)

runA :: String -> Int
runA = searchA . toState . parse

test :: ()
test
  | runA testData /= 12521 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap runA $ readFile "input/23.txt"

type State2 = (Map Int Char,Map Int (Char,Char,Char,Char))

toState2 :: Map (Int,Int) Char -> State2
toState2 burrow = (fromList [(x,'.') | x <- [1..11]],fromList [
    (3,(burrow!(3,2),'D','D',burrow!(3,3))),
    (5,(burrow!(5,2),'C','B',burrow!(5,3))),
    (7,(burrow!(7,2),'B','A',burrow!(7,3))),
    (9,(burrow!(9,2),'A','C',burrow!(9,3)))])

searchA2 :: State2 -> Int
searchA2 initialState =
    astar ((fromList [((h initialState,initialState),())]),
           (fromList [(initialState,h initialState)]),
           (fromList [(initialState,0)]))
  where
    factor '.' = 0
    factor 'A' = 1
    factor 'B' = 10
    factor 'C' = 100
    factor 'D' = 1000
    home '.' = 0
    home 'A' = 3
    home 'B' = 5
    home 'C' = 7
    home 'D' = 9

    goal = fromList [(3,('A','A','A','A')),(5,('B','B','B','B')),(7,('C','C','C','C')),(9,('D','D','D','D'))]

    h :: State2 -> Int
    h (hall,homes) = sum (mapWithKey hHall hall) + sum (mapWithKey hHome homes)
    hHall x c = factor c*(abs (x - home c) + 1)
    hHome x (c1,c2,c3,c4)
      | home c4 == x && home c3 == x && home c2 == x && home c1 == x = 0
      | home c4 == x && home c3 == x && home c2 == x = factor c1*(abs (x - home c1) + 2)
      | home c4 == x && home c3 == x = factor c2*(abs (x - home c2) + 3) + factor c1*(if home c1 == x then 5 else x - home c1 + 2)
      | home c4 == x = factor c3*(abs (x - home c3) + 4) + factor c2*(if home c2 == x then 7 else x - home c2 + 3) + factor c1*(if home c1 == x then 5 else x - home c1 + 2)
      | otherwise = factor c4*(abs (x - home c4) + 5) + factor c3*(if home c3 == x then 9 else x - home c3 + 4) + factor c2*(if home c2 == x then 7 else x - home c2 + 3) + factor c1*(if home c1 == x then 5 else x - home c1 + 2)

    neighbors :: State2 -> [(Int,State2)]
    neighbors state@(hall,homes) = concatMap (moveHome state) (toList hall) ++ concatMap (moveOut state) (toList homes)

    moveHome :: State2 -> (Int,Char) -> [(Int,State2)]
    moveHome state@(hall,homes) (x,c)
      | c == '.' = []
      | home1 /= '.' = []
      | home2 /= '.' && home2 /= c = []
      | home3 /= '.' && home3 /= c = []
      | home4 /= '.' && home4 /= c = []
      | home c > x && and [hall!xx == '.' | xx <- [x+1..home c]] =
          if home4 == '.' then
              [(factor c*(home c-x+4),(insert x '.' hall,insert (home c) ('.','.','.',c) homes))]
          else if home3 == '.' then
              [(factor c*(home c-x+3),(insert x '.' hall,insert (home c) ('.','.',c,c) homes))]
          else if home2 == '.' then
              [(factor c*(home c-x+2),(insert x '.' hall,insert (home c) ('.',c,c,c) homes))]
          else
              [(factor c*(home c-x+1),(insert x '.' hall,insert (home c) (c,c,c,c) homes))]
      | home c < x && and [hall!xx == '.' | xx <- [home c..x-1]] =
          if home4 == '.' then
              [(factor c*(x-home c+4),(insert x '.' hall,insert (home c) ('.','.','.',c) homes))]
          else if home3 == '.' then
              [(factor c*(x-home c+3),(insert x '.' hall,insert (home c) ('.','.',c,c) homes))]
          else if home2 == '.' then
              [(factor c*(x-home c+2),(insert x '.' hall,insert (home c) ('.',c,c,c) homes))]
          else
              [(factor c*(x-home c+1),(insert x '.' hall,insert (home c) (c,c,c,c) homes))]
      | otherwise = []
      where
        (home1,home2,home3,home4) = homes!(home c)

    moveOut :: State2 -> (Int,(Char,Char,Char,Char)) -> [(Int,State2)]
    moveOut state@(hall,homes) (x,(c1,c2,c3,c4))
      | home c1 == x && home c2 == x && home c3 == x && home c4 == x = []
      | c1 == '.' && home c2 == x && home c3 == x && home c4 == x = []
      | c1 == '.' && c2 == '.' && home c3 == x && home c4 == x = []
      | c1 == '.' && c2 == '.' && c3 == '.' && home c4 == x = []
      | c1 == '.' && c2 == '.' && c3 == '.' && c4 == '.' = []
      | c1 /= '.' = [(factor c1*(abs (x-xx)+1),(insert xx c1 hall,insert x ('.',c2,c3,c4) homes)) | xx <- destXs]
      | c2 /= '.' = [(factor c2*(abs (x-xx)+2),(insert xx c2 hall,insert x ('.','.',c3,c4) homes)) | xx <- destXs]
      | c3 /= '.' = [(factor c3*(abs (x-xx)+3),(insert xx c3 hall,insert x ('.','.','.',c4) homes)) | xx <- destXs]
      | otherwise = [(factor c4*(abs (x-xx)+4),(insert xx c4 hall,insert x ('.','.','.','.') homes)) | xx <- destXs]
      where
        destXs = takeWhile ((== '.') . (hall!)) [xx | xx <- [x+1..11], xx /= 3, xx /= 5, xx /= 7, xx /= 9] ++ takeWhile ((== '.') . (hall!)) [xx | xx <- [x-1,x-2..1], xx /= 3, xx /= 5, xx /= 7, xx /= 9]

    astar :: (Map (Int,State2) (),Map State2 Int,Map State2 Int) -> Int
    astar (openQ,openSet,scores)
      | currentHomes == goal = currentScore
      | otherwise = astar $ foldr (handleNeighbor currentScore) (newOpenQ,newOpenSet,scores) (neighbors current)
      where
        Just (((_,current@(_,currentHomes)),()),newOpenQ) = minViewWithKey openQ
        newOpenSet = delete current openSet
        currentScore = scores!current

    handleNeighbor :: Int -> (Int,State2) -> (Map (Int,State2) (),Map State2 Int,Map State2 Int) -> (Map (Int,State2) (),Map State2 Int,Map State2 Int)
    handleNeighbor currentScore (dscore,state) (openQ,openSet,scores)
      | maybe False ((currentScore+dscore) >=) $ Data.Map.lookup state scores = (openQ,openSet,scores)
      | Data.Map.member state openSet = (insert ((currentScore+dscore+h state),state) () $ delete (openSet!state,state) openQ,insert state (currentScore+dscore+h state) openSet,insert state (currentScore+dscore) scores)
      | otherwise = (insert ((currentScore+dscore+h state),state) () openQ,insert state (currentScore+dscore+h state) openSet,insert state (currentScore+dscore) scores)

runA2 :: String -> Int
runA2 = searchA2 . toState2 . parse

testA2 :: ()
testA2
  | runA2 testData /= 44169 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap runA2 $ readFile "input/23.txt"
