import Debug.Trace(traceShow)

import Data.Set(Set,fromList,insert,member,minView,notMember,size)
import qualified Data.Set

type Valley = ((Int,Int),Set (Int,Int),Set (Int,Int),Set (Int,Int),Set (Int,Int))

parse :: String -> Valley
parse = findEntrance 0 0
  where
    findEntrance x y (c:rest)
      | c == '\n' = findEntrance 0 (y+1) rest
      | c == '.' = makeValley (x+1) y (x,y) [] [] [] [] (x,y) rest
      | otherwise = findEntrance (x+1) y rest
    makeValley _ _ _ n s e w xyExit [] =
        (xyExit,fromList n,fromList s,fromList e,fromList w)
    makeValley x y xy0@(x0,y0) n s e w xyExit (c:rest)
      | c == '\n' = makeValley 0 (y+1) xy0 n s e w xyExit rest
      | c == '.' = makeValley (x+1) y xy0 n s e w (x-x0,y-y0) rest
      | c == '^' = makeValley (x+1) y xy0 ((x-x0,y-y0):n) s e w xyExit rest
      | c == 'v' = makeValley (x+1) y xy0 n ((x-x0,y-y0):s) e w xyExit rest
      | c == '<' = makeValley (x+1) y xy0 n s e ((x-x0,y-y0):w) xyExit rest
      | c == '>' = makeValley (x+1) y xy0 n s ((x-x0,y-y0):e) w xyExit rest
      | otherwise = makeValley (x+1) y xy0 n s e w xyExit rest

moveBlizzards :: Valley -> Valley
moveBlizzards (maxXY@(maxX,maxY),n,s,e,w) = (maxXY,Data.Set.map goN n,Data.Set.map goS s,Data.Set.map goE e,Data.Set.map goW w)
  where
    goN (x,y) = (x,(y-1-1) `mod` (maxY-1) + 1) -- y goes from 1 to maxY-1
    goS (x,y) = (x,(y+1-1) `mod` (maxY-1) + 1)
    goE (x,y) = ((x+1) `mod` (maxX+1),y) -- x goes from 0 to maxX
    goW (x,y) = ((x-1) `mod` (maxX+1),y)

type State = (Int,((Int,Int),Valley))

-- Example (minute 5) allows swapping spaces with a blizzard.
choices :: State -> [State]
choices (t,(oldXY@(x,y),oldBlizzards)) =
    [(t+1,(xy,blizzards))
        | xy@(newX,newY) <- [(x,y),(x+1,y),(x,y+1),(x-1,y),(x,y-1)],
          notMember xy n, notMember xy s, notMember xy e, notMember xy w,
          newX >= 0, newY > 0 || xy == (0,0), newX <= maxX, newY < maxY || xy == (maxX,maxY)]
  where
    blizzards@((maxX,maxY),n,s,e,w) = moveBlizzards oldBlizzards

search :: Valley -> Int
search initialValley = astar 0 (fromList [(h initialState,initialState)])
  where
    initialState = (0,((0,0),initialValley))

    h :: State -> Int
    h (t,((x,y),((maxX,maxY),_,_,_,_))) = t + abs (maxX-x) + abs (maxY-y)

    astar :: Int -> Set (Int,State) -> Int
    astar maxT open
      | t > maxT && traceShow (size open,xy,t) False = undefined
      | xy == exit = t
      | otherwise = astar (max t maxT) $ foldr insert newOpen $ map (\ s -> (h s,s)) $ choices state
      where
        Just ((_,state@(t,(xy,(exit,n,s,e,w)))),newOpen) = minView open

display :: State -> String
display (_,(xyE,((xMax,yMax),n,s,e,w))) = unlines (
    (if xyE == (0,0) then "E" else "."):
    [[if xyE == xy then 'E'
      else if count xy == 4 then '4'
      else if count xy == 3 then '3'
      else if count xy == 2 then '2'
      else if member xy n then '^'
      else if member xy s then 'v'
      else if member xy e then '>'
      else if member xy w then '<'
      else '.' | xy <- map (flip (,) y) [0..xMax]] | y <- [1..yMax-1]])
  where count xy = length $ filter (member xy) [n,s,e,w]

testData :: [String]
testData = map unlines [[
    "#.#####",
    "#.....#",
    "#>....#",
    "#.....#",
    "#...v.#",
    "#.....#",
    "#####.#"
    ],[
    "#.######",
    "#>>.<^<#",
    "#.<..<<#",
    "#>v.><>#",
    "#<^v^^>#",
    "######.#"
    ]
    ]

test :: ()
test
  | (search . parse) (testData !! 0) /= 10 = error "a"
  | (search . parse) (testData !! 1) /= 18 = error "b"
  | otherwise = ()

-- This is slow.
part1 :: IO Int
part1 = fmap (search . parse) $ readFile "input/24.txt"

search2 :: Valley -> (Int,Int) -> (Int,Int) -> (Int,Valley)
search2 initialValley startXY goalXY@(goalX,goalY) = astar 0 (fromList [(h initialState,initialState)])
  where
    initialState = (0,(startXY,initialValley))

    h :: State -> Int
    h (t,((x,y),_)) = t + abs (goalX-x) + abs (goalY-y)

    astar :: Int -> Set (Int,State) -> (Int,Valley)
    astar maxT open
      | t > maxT && traceShow (size open,xy,t) False = undefined
      | xy == goalXY = (t,valley)
      | otherwise = astar (max t maxT) $ foldr insert newOpen $ map (\ s -> (h s,s)) $ choices state
      where
        Just ((_,state@(t,(xy,valley))),newOpen) = minView open

-- 3 times slower than part 1
run2 :: String -> Int
run2 input = t1 + t2 + t3
  where
    valley@(goal,_,_,_,_) = parse input
    (t1,valley2) = search2 valley (0,0) goal
    (t2,valley3) = search2 valley2 goal (0,0)
    (t3,_) = search2 valley3 (0,0) goal

test2 :: ()
test2
  | run2 (testData !! 1) /= 54 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap run2 $ readFile "input/24.txt"
