module AOC202217 where

import Data.Map(Map,empty,insert,mapWithKey,member,(!))
import qualified Data.Map
import Data.Set(Set,difference,elems,fromList,intersection,maxView,singleton,union)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2022/input/17",
    aocTests=[
        AOCTest {
            testData=unlines [
                ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
                ],
            testResult=Just "3068",
            testResult2=Just "1514285714288"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse = filter (`elem` "<>")

type Shape = (Int,Int,Set Int) -- x, xmax, shape

shapeList :: [Shape]
shapeList = [
    (2,3,fromList [2,3,4,5]), -- ####     --  #
    (2,4,fromList [2*7+3,7+2,7+3,7+4,3]), -- ###  --   #  -- #
                                          --  #   --   #  -- #
    (2,4,fromList [2*7+4,7+4,2,3,4]),             -- ###  -- #
    (2,6,fromList [3*7+2,2*7+2,7+2,2]),                   -- #  -- ##
    (2,5,fromList [7+2,7+3,2,3])                                -- ##
    ]

moveShape :: Int -> Int -> Shape -> Shape
moveShape dx dy (x,xmax,blocks) = (x+dx,xmax,Data.Set.map (+ (dx+7*dy)) blocks)

initialTower :: Set Int
initialTower = fromList [0..6]

dropRock :: (jet -> Bool) -> (Set Int,([Shape],[jet]))
         -> (Set Int,([Shape],[jet]))
dropRock isLeft (tower,(shape:shapes,jets)) = jet (moveShape 0 y0 shape) jets
  where
    y0 = 4 + maximum tower `div` 7
    jet shape (j:js)
      | isLeft j = fall (moveShape (-1) 0 shape)
      | otherwise = fall (moveShape 1 0 shape)
      where
        fall movedShape@(x,xmax,blocks)
          | x >= 0 && x <= xmax && null (intersection blocks tower) =
              continue movedShape
          | otherwise = continue shape
        continue jettedShape@(_,_,jettedBlocks)
          | null (intersection droppedBlocks tower) = jet droppedShape js
          | otherwise = (union jettedBlocks tower,(shapes,js))
          where droppedShape@(_,_,droppedBlocks) = moveShape 0 (-1) jettedShape

result jets =
    (`div` 7) $ maximum $ fst $ head $ drop 2022
              $ iterate (dropRock (== '<'))
                        (initialTower,(cycle shapeList,cycle jets))

shiftDown :: Int -> (Int,Set Int) -> (Int,Set Int)
shiftDown shiftCount (bottom,tower) =
    (bottom+shiftCount,
     Data.Set.filter (>= 0) $ Data.Set.map (flip (-) (7*shiftCount)) tower)

-- Look for the highest rocks that join the left and right sides:
-- Additional rocks cannot be affected by rocks lower than the lowest
-- of those rocks, so they can be removed from the chamber when looking
-- for cycles in the tower.
getShiftCount :: Set Int -> Int
getShiftCount tower =
    search Data.Set.empty False False (singleton (maximum tower))
  where
    search seen leftSeen rightSeen queue
      | leftSeen && rightSeen = minimum seen `div` 7
      | isLeft = search nextSeen True rightSeen nextQueueL
      | isRight = search nextSeen leftSeen True nextQueueR
      | otherwise = search nextSeen leftSeen rightSeen nextQueueLR
      where
        Just (xy,nextQueue) = maxView queue
        isLeft = xy `mod` 7 == 0
        isRight = xy `mod` 7 == 6
        nextSeen = Data.Set.insert xy seen
        nextQueueL =
            union nextQueue (difference (intersection enqueueL tower) seen)
        nextQueueR =
            union nextQueue (difference (intersection enqueueR tower) seen)
        nextQueueLR =
            union nextQueue (difference (intersection enqueueLR tower) seen)
        enqueueL = fromList [xy+7,xy+7+1,xy+1,xy-7,xy-7+1]
        enqueueR = fromList [xy+7,xy+7-1,xy-1,xy-7,xy-7-1]
        enqueueLR = union nextQueueL nextQueueR

dropRock2 :: ([Shape],[(Int,Char)],(Int,Set Int))
          -> ([Shape],[(Int,Char)],(Int,Set Int))
dropRock2 (shapes,jets,(bottom,tower)) = (nextShapes,nextJets,nextChamber)
  where
    (nextTower,(nextShapes,nextJets)) =
        dropRock ((== '<') . snd) (tower,(shapes,jets))
    shiftCount = getShiftCount nextTower
    nextChamber
      | shiftCount == 0 = (bottom,nextTower)
      | otherwise = shiftDown shiftCount (bottom,nextTower)

height :: (Int,Set Int) -> Int
height (bottom,tower) = bottom + (maximum tower `div` 7)

findCycle :: Int -> [Shape] -> [(Int,Char)] -> (Int,Set Int)
          -> Map (Shape,Int,Set Int) (Int,Int) -> Map Int Int
          -> (Int,Int,Int,Map Int Int)
findCycle n shapes@(shape:_) jets@((jetIndex,_):_) chamber@(_,tower)
          states heights
  | member state states =
      (cycleStart,n - cycleStart,currentHeight - cycleStartHeight,heights)
  | otherwise =
      findCycle (n+1) nextShapes nextJets nextChamber
                (insert state (n,currentHeight) states)
                (insert n currentHeight heights)
  where
    state = (shape,jetIndex,tower)
    (cycleStart,cycleStartHeight) = states!state

    currentHeight = height chamber
    (nextShapes,nextJets,nextChamber) = dropRock2 (shapes,jets,chamber)

result2 jets = heights!index + nCycles*cycleHeight
  where
    nCycles = (1000000000000 - cycleStart) `div` cycleLength
    index = (1000000000000 - cycleStart) `mod` cycleLength + cycleStart
    (cycleStart,cycleLength,cycleHeight,heights) =
        findCycle 0 (cycle shapeList) (cycle $ zip [0..] jets)
                  (0,initialTower) empty empty
