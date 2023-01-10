{-
--- Day 22: Mode Maze ---

This is it, your final stop: the year -483. It's snowing and dark outside; the
only light you can see is coming from a small cottage in the distance. You make
your way there and knock on the door.

A portly man with a large, white beard answers the door and invites you inside.
For someone living near the North Pole in -483, he must not get many visitors,
but he doesn't act surprised to see you. Instead, he offers you some milk and
cookies.

After talking for a while, he asks a favor of you. His friend hasn't come back
in a few hours, and he's not sure where he is. Scanning the region briefly, you
discover one life signal in a cave system nearby; his friend must have taken
shelter there. The man asks if you can go there to retrieve his friend.

The cave is divided into square regions which are either dominantly rocky,
narrow, or wet (called its type). Each region occupies exactly one coordinate
in X,Y format where X and Y are integers and zero or greater. (Adjacent
regions can be the same type.)

The scan (your puzzle input) is not very detailed: it only reveals the depth of
the cave system and the coordinates of the target. However, it does not reveal
the type of each region. The mouth of the cave is at 0,0.

The man explains that due to the unusual geology in the area, there is a method
to determine any region's type based on its erosion level. The erosion level of
a region can be determined from its geologic index. The geologic index can be
determined using the first rule that applies from the list below:

 - The region at 0,0 (the mouth of the cave) has a geologic index of 0.
 - The region at the coordinates of the target has a geologic index of 0.
 - If the region's Y coordinate is 0, the geologic index is its X coordinate
   times 16807.
 - If the region's X coordinate is 0, the geologic index is its Y coordinate
   times 48271.
 - Otherwise, the region's geologic index is the result of multiplying the
   erosion levels of the regions at X-1,Y and X,Y-1.

A region's erosion level is its geologic index plus the cave system's depth,
all modulo 20183. Then:

 - If the erosion level modulo 3 is 0, the region's type is rocky.
 - If the erosion level modulo 3 is 1, the region's type is wet.
 - If the erosion level modulo 3 is 2, the region's type is narrow.

For example, suppose the cave system's depth is 510 and the target's
coordinates are 10,10. Using % to represent the modulo operator, the cavern
would look as follows:

 - At 0,0, the geologic index is 0. The erosion level is (0 + 510) % 20183 =
   510. The type is 510 % 3 = 0, rocky.
 - At 1,0, because the Y coordinate is 0, the geologic index is 1 * 16807 =
   16807. The erosion level is (16807 + 510) % 20183 = 17317. The type is
   17317 % 3 = 1, wet.
 - At 0,1, because the X coordinate is 0, the geologic index is 1 * 48271 =
   48271. The erosion level is (48271 + 510) % 20183 = 8415. The type is
   8415 % 3 = 0, rocky.
 - At 1,1, neither coordinate is 0 and it is not the coordinate of the target,
   so the geologic index is the erosion level of 0,1 (8415) times the erosion
   level of 1,0 (17317), 8415 * 17317 = 145722555. The erosion level is
   (145722555 + 510) % 20183 = 1805. The type is 1805 % 3 = 2, narrow.
 - At 10,10, because they are the target's coordinates, the geologic index is
   0. The erosion level is (0 + 510) % 20183 = 510. The type is 510 % 3 = 0,
   rocky.

Drawing this same cave system with rocky as ., wet as =, narrow as |, the mouth
as M, the target as T, with 0,0 in the top-left corner, X increasing to the
right, and Y increasing downward, the top-left corner of the map looks like
this:

| M=.|=.|.|=.|=|=.
| .|=|=|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..===..=|.|||.
| .======|||=|=.|=
| .===|=|===T===||
| =|||...|==..|=.|
| =.=|=.=..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||

Before you go in, you should determine the risk level of the area. For the
rectangle that has a top-left corner of region 0,0 and a bottom-right corner of
the region containing the target, add up the risk level of each individual
region: 0 for rocky regions, 1 for wet regions, and 2 for narrow regions.

In the cave system above, because the mouth is at 0,0 and the target is at
10,10, adding up the risk level of all regions with an X coordinate from 0 to
10 and a Y coordinate from 0 to 10, this total is 114.

What is the total risk level for the smallest rectangle that includes 0,0 and
the target's coordinates?
-}

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

{-
--- Part Two ---

Okay, it's time to go rescue the man's friend.

As you leave, he hands you some tools: a torch and some climbing gear. You
can't equip both tools at once, but you can choose to use neither.

Tools can only be used in certain regions:

 - In rocky regions, you can use the climbing gear or the torch. You cannot use
   neither (you'll likely slip and fall).
 - In wet regions, you can use the climbing gear or neither tool. You cannot
   use the torch (if it gets wet, you won't have a light source).
 - In narrow regions, you can use the torch or neither tool. You cannot use the
   climbing gear (it's too bulky to fit).

You start at 0,0 (the mouth of the cave) with the torch equipped and must reach
the target coordinates as quickly as possible. The regions with negative X or Y
are solid rock and cannot be traversed. The fastest route might involve
entering regions beyond the X or Y coordinate of the target.

You can move to an adjacent region (up, down, left, or right; never diagonally)
if your currently equipped tool allows you to enter that region. Moving to an
adjacent region takes one minute. (For example, if you have the torch equipped,
you can move between rocky and narrow regions, but cannot enter wet regions.)

You can change your currently equipped tool or put both away if your new
equipment would be valid for your current region. Switching to using the
climbing gear, torch, or neither always takes seven minutes, regardless of
which tools you start with. (For example, if you are in a rocky region, you can
switch from the torch to the climbing gear, but you cannot switch to neither.)

Finally, once you reach the target, you need the torch equipped before you can
find him in the dark. The target is always in a rocky region, so if you arrive
there with climbing gear equipped, you will need to spend seven minutes
switching to your torch.

For example, using the same cave system as above, starting in the top left
corner (0,0) and moving to the bottom right corner (the target, 10,10) as
quickly as possible, one possible route is as follows, with your current
position marked X:

| Initially:
| X=.|=.|.|=.|=|=.
| .|=|=|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..===..=|.|||.
| .======|||=|=.|=
| .===|=|===T===||
| =|||...|==..|=.|
| =.=|=.=..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||
| 
| Down:
| M=.|=.|.|=.|=|=.
| X|=|=|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..===..=|.|||.
| .======|||=|=.|=
| .===|=|===T===||
| =|||...|==..|=.|
| =.=|=.=..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||
| 
| Right:
| M=.|=.|.|=.|=|=.
| .X=|=|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..===..=|.|||.
| .======|||=|=.|=
| .===|=|===T===||
| =|||...|==..|=.|
| =.=|=.=..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||
| 
| Switch from using the torch to neither tool:
| M=.|=.|.|=.|=|=.
| .X=|=|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..===..=|.|||.
| .======|||=|=.|=
| .===|=|===T===||
| =|||...|==..|=.|
| =.=|=.=..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||
| 
| Right 3:
| M=.|=.|.|=.|=|=.
| .|=|X|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..===..=|.|||.
| .======|||=|=.|=
| .===|=|===T===||
| =|||...|==..|=.|
| =.=|=.=..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||
| 
| Switch from using neither tool to the climbing gear:
| M=.|=.|.|=.|=|=.
| .|=|X|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..===..=|.|||.
| .======|||=|=.|=
| .===|=|===T===||
| =|||...|==..|=.|
| =.=|=.=..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||
| 
| Down 7:
| M=.|=.|.|=.|=|=.
| .|=|=|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..X==..=|.|||.
| .======|||=|=.|=
| .===|=|===T===||
| =|||...|==..|=.|
| =.=|=.=..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||
| 
| Right:
| M=.|=.|.|=.|=|=.
| .|=|=|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..=X=..=|.|||.
| .======|||=|=.|=
| .===|=|===T===||
| =|||...|==..|=.|
| =.=|=.=..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||
| 
| Down 3:
| M=.|=.|.|=.|=|=.
| .|=|=|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..===..=|.|||.
| .======|||=|=.|=
| .===|=|===T===||
| =|||.X.|==..|=.|
| =.=|=.=..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||
| 
| Right:
| M=.|=.|.|=.|=|=.
| .|=|=|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..===..=|.|||.
| .======|||=|=.|=
| .===|=|===T===||
| =|||..X|==..|=.|
| =.=|=.=..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||
| 
| Down:
| M=.|=.|.|=.|=|=.
| .|=|=|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..===..=|.|||.
| .======|||=|=.|=
| .===|=|===T===||
| =|||...|==..|=.|
| =.=|=.X..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||
| 
| Right 4:
| M=.|=.|.|=.|=|=.
| .|=|=|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..===..=|.|||.
| .======|||=|=.|=
| .===|=|===T===||
| =|||...|==..|=.|
| =.=|=.=..=X||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||
| 
| Up 2:
| M=.|=.|.|=.|=|=.
| .|=|=|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..===..=|.|||.
| .======|||=|=.|=
| .===|=|===X===||
| =|||...|==..|=.|
| =.=|=.=..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||
| 
| Switch from using the climbing gear to the torch:
| M=.|=.|.|=.|=|=.
| .|=|=|||..|.=...
| .==|....||=..|==
| =.|....|.==.|==.
| =|..==...=.|==..
| =||.=.=||=|=..|=
| |.=.===|||..=..|
| |..==||=.|==|===
| .=..===..=|.|||.
| .======|||=|=.|=
| .===|=|===X===||
| =|||...|==..|=.|
| =.=|=.=..=.||==|
| ||=|=...|==.=|==
| |=.=||===.|||===
| ||.|==.|.|.||=||

This is tied with other routes as the fastest way to reach the target: 45
minutes. In it, 21 minutes are spent switching tools (three times, seven
minutes each) and the remaining 24 minutes are spent moving.

What is the fewest number of minutes you can take to reach the target?
-}

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
