import Data.List(sort)
import Data.Map(Map,elems,empty,member,fromList,insert,(!))
import qualified Data.Map

makeCave :: (Int,(Int,Int)) -> (Int,Int) -> Map (Int,Int) Int
makeCave (depth,target) (xmax,ymax) = cave
  where
    cave = fromList [((x,y),geologicalIndex (x,y)) | x <- [0..xmax], y <- [0..ymax]]
    geologicalIndex xy@(x,y)
      | xy == (0,0) || xy == target = 0
      | y == 0 = x*16807
      | x == 0 = y*48271
      | otherwise = erosionLevel depth (cave!(x-1,y)) * erosionLevel depth (cave!(x,y-1))

erosionLevel :: Int -> Int -> Int
erosionLevel depth geologicalIndex = (geologicalIndex + depth) `mod` 20183

regionType :: Int -> Int -> Int
regionType depth geologicalIndex = erosionLevel depth geologicalIndex `mod` 3

test :: ()
test
  | cave!(0,0) /= 0 = error "a"
  | erosionLevel 510 (cave!(0,0)) /= 510 = error "b"
  | cave!(1,0) /= 16807 = error "c"
  | erosionLevel 510 (cave!(1,0)) /= 17317 = error "d"
  | cave!(0,1) /= 48271 = error "e"
  | erosionLevel 510 (cave!(0,1)) /= 8415 = error "f"
  | cave!(1,1) /= 145722555 = error "g"
  | erosionLevel 510 (cave!(1,1)) /= 1805 = error "h"
  | cave!(10,10) /= 0 = error "i"
  | erosionLevel 510 (cave!(10,10)) /= 510 = error "j"
  | sum (map (regionType 510) $ elems cave) /= 114 = error "k"
  | otherwise = ()
  where cave = makeCave (510,(10,10)) (10,10)

part1 :: (Int,(Int,Int)) -> Int
part1 (depth,target) =
    sum (map (regionType depth) $ elems $ makeCave (depth,target) target)

data Region = Rocky | Wet | Narrow deriving (Eq, Show)

type Cave = Map (Int,Int) Region

data Equip = Climb | Torch | Neither deriving (Eq, Ord, Show)

type Survey = Map (Equip,(Int,Int)) Int

type State = (Int,(Equip,(Int,Int)))

toCave :: Int -> Map (Int,Int) Int -> Cave
toCave depth = Data.Map.map (toRegion . regionType depth)
  where
    toRegion 0 = Rocky
    toRegion 1 = Wet
    toRegion 2 = Narrow

equipValid :: Region -> Equip -> Bool
equipValid Rocky Neither = False
equipValid Wet Torch = False
equipValid Narrow Climb = False
equipValid _ _ = True

-- This breadth-first search is unsatisfactorily slow.
-- Perhaps searching from both the start and end until they
-- meet would be faster, but that would be more complicated due to 
-- longer paths possibly being faster due to the reequip time.
-- Something else that might help would be to use the straightest
-- path as an upper bound before starting the search.
search :: (Equip,(Int,Int)) -> Cave -> Int
search target cave = doSurvey target cave (Nothing,empty,[(0,(Torch,(0,0)))])

doSurvey :: (Equip,(Int,Int)) -> Cave -> (Maybe Int,Survey,[State]) -> Int
doSurvey target cave (minTime,survey,states)
  | null states = maybe (error "?") id minTime
  | otherwise = doSurvey target cave $ foldr (doSurvey1 target cave) (minTime,survey,[]) (sort states)

doSurvey1 :: (Equip,(Int,Int)) -> Cave -> State -> (Maybe Int,Survey,[State]) -> (Maybe Int,Survey,[State])
doSurvey1 target cave state@(time,pos@(equip,xy)) (minTime,survey,states) =
    (newMinTime,newSurvey,newStates ++ states)
  where
    newMinTime
      | pos == target && maybe True (> time) minTime = Just time
      | otherwise = minTime
    newStates = filter (validSearch cave minTime survey) (nextStates state)
    newSurvey = foldr add survey newStates
    add (time,pos) survey = insert pos time survey

nextStates :: State -> [State]
nextStates (time,(equip,(x,y))) =
    [(time+1,(equip,xy)) | xy <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]] ++
    [(time+7,(newEquip,(x,y))) | newEquip <- [Neither,Torch,Climb],
                                 newEquip /= equip]

validSearch :: Cave -> Maybe Int -> Survey -> State -> Bool
validSearch cave minTime survey (time,pos@(equip,xy))
  | maybe False (<= time) minTime = False
  | not (member xy cave) = False
  | not (equipValid (cave!xy) equip) = False
  | member pos survey && time >= survey!pos = False
  | otherwise = True

part2 :: (Int,(Int,Int)) -> Int
part2 (depth,target@(x,y)) = search (Torch,target) cave
  where
    cave = toCave depth (makeCave (depth,target) (x+50,y+50))

test2 :: ()
test2
  | part2 (510,(10,10)) /= 45 = error "a"
  | otherwise = ()
